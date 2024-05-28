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
