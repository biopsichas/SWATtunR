#' Calculate concentrations
#'
#' This function calculates concentrations based on simulated data, adjusting
#' for specified parameters.
#'
#' @param sim A simulation object containing the data.
#' @param not_conc (optional) Regular expression pattern for variables that should not be
#' considered for concentration calculation. Default \code{not_conc = '^flo_day|^gwd'}.
#' @param sediment (optional) Regular expression pattern for sediment variables.
#' Default \code{sediment = '^sed_'}.
#' @importFrom dplyr bind_cols
#' @importFrom purrr map2 set_names
#' @return A modified simulation object with added concentration variables.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' sim_data <- get_conc(sim_data)
#' }
#' @keywords data manipulation

get_conc <- function(sim, not_conc = '^flo_day|^gwd', sediment = '^sed_'){
  if(!any(grepl("flo_day", names(sim$simulation)))){
    stop("Please make sure that the simulation object contains a variable called 'flo_day'!!!")
  }
  # Extract the names of the variables that should be used for the calculation
  names_par <- names(sim$simulation)[!grepl(not_conc, names(sim$simulation))]

  # Rename the variables to have the same names as the flow variables.
  names_flow_par <- sub("^[^_]*", "flo", names_par)

  not_match <- names(sim$simulation)[!grepl(not_conc, names(sim$simulation))][!names_flow_par %in% names(sim$simulation)]
  if(length(not_match)){
    msg <- paste0("The following variable/s names: ", paste(not_match, collapse = ", "), " \n ",
                  " do not match the flow variable/s names: ",
                  paste(names(sim$simulation)[grepl("^flo", names(sim$simulation))], collapse = ", "), " \n ",
                  "Please check the names of the variables and rename to match flow variable names!, \n")
    stop(cat(msg))
  }
  # Calculate the concentrations from the simulated data.
  sim_conc <- map2(names_par, names_flow_par , ~{bind_cols(sim$simulation[[.x]][,1],
                                                           (sim$simulation[[.x]][,-1]/sim$simulation[[.y]][,-1])*0.01157407)}) %>%
    set_names(., paste0(names_par, "_conc"))

  # Add the calculated concentrations to the simulation object.
  sim$simulation[names(sim_conc)] <- sim_conc

  # Sediments are in tons, so they need to be converted to kg.
  if(any(grepl(paste0(sediment,'.*\\conc$'), names(sim$simulation)))){
    sim$simulation[[names(sim$simulation)[grepl(paste0(sediment,'.*\\conc$'), names(sim$simulation))]]] <-
      bind_cols(sim$simulation[[1]][,1],
                sim$simulation[[names(sim$simulation)[grepl(paste0(sediment,'.*\\conc$'), names(sim$simulation))]]][,-1] * 1000)
  }
  return(sim)
}


#' Sample parameters space
#'
#' This function samples the parameter space using Latin Hypercube Sampling (LHS).
#'
#' @param par A matrix or data frame specifying the parameter ranges.
#' @param n The number of samples to generate.
#'
#' @return A tibble containing the sampled parameter values.
#' @importFrom lhs randomLHS
#' @importFrom dplyr %>%
#' @importFrom purrr map2_df set_names
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' # Define parameter ranges
#' parameters <- data.frame(
#'   param1 = c(0, 1),
#'   param2 = c(0, 2),
#'   param3 = c(0, 10)
#' )
#'
#' # Sample the parameter space
#' sample_space <- sample_lhs(parameters, 3)
#' sample_space
#' @keywords sampling

sample_lhs <- function(par, n) {
  n_par <- ncol(par)

  randomLHS(n = n, k = n_par) %>% # Perform sampling
    as_tibble(., .name_repair = 'minimal') %>% # Convert to a tibble
    set_names(names(par)) %>% # Assign the parameter names with purrr
    map2_df(., par, ~ (.x * (.y[2] - .y[1]) + .y[1])) # Scale parameter ranges
}


#' Calculate Water Yield Ratio (WYR)
#'
#' This function calculates the Water Yield Ratio (WYR) based on various components
#' of a simulation, including precipitation, surface runoff, lateral flow, and
#' others. It aggregates these components and computes the ratio of the sum of
#' water yields to the precipitation.
#'
#' @param sim A SWATrunR nested list containing simulation data. It should have a named element
#' 'simulation' which itself contains named elements: 'precip', 'surq_cha', 'surq_res',
#' 'latq_cha', 'latq_res', 'qtile', and 'flo'.
#'
#' @return A numeric value representing the Water Yield Ratio (WYR).
#' @export
#' @examples
#' \dontrun{
#' # Calculate WYR
#' wyr <- calculate_wyr(sim)
#' }
#' @keywords calculate

calculate_wyr <- function(sim) {
  precip  <- aggregate_aa(sim$simulation$precip)
  surq_cha <- aggregate_aa(sim$simulation$surq_cha)
  surq_res <- aggregate_aa(sim$simulation$surq_res)
  latq_cha <- aggregate_aa(sim$simulation$latq_cha)
  latq_res <- aggregate_aa(sim$simulation$latq_res)
  qtile    <- aggregate_aa(sim$simulation$qtile)
  flo      <- aggregate_aa(sim$simulation$flo)
  wyr <- (surq_cha + surq_res + latq_cha + latq_res + qtile + flo) / precip

  return(wyr)
}
