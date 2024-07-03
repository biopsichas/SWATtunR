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
  if(any(grepl("_conc", names(sim$simulation)))){
    stop("The simulation object already contains a concentration variable/s ending with '_conc'!!! Please remove them before running 'get_conc().'")
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

#' Calculate All Performance Metrics
#'
#' This function calculates various performance metrics for a given simulation
#' and observed data.
#'
#' @param sim Object from SWATrunR
#' @param obs Dataframe for the observed data with two columns: date and value.
#' @param par_name (optional) Character for name of the parameter set to be used.
#' Default \code{par_name = NULL}, which select first variable in sim object.
#' But if you have multiple parameter sets, you can use this argument to select
#' the one you want to use. Example par_name = "flo_day" or par_name = no3_day_conc".
#' @param perf_metrics (optional) Character vector with the names of the performance metrics
#' to used in the rank_tot calculation. Default \code{perf_metrics = NULL},
#' which means that all performance metrics will be used in calculation.
#' Other example could be perf_metrics = c("kge", "nse"), which means that only
#' KGE and NSE will be used in calculation.
#' @param period (optional) Character describing, which time interval to display.
#' Default \code{period = NULL}, which mean not activated, other examples are
#' "day", "week", "month", etc).
#' See [lubridate::floor_date](https://www.rdocumentation.org/packages/lubridate/versions/1.3.3/topics/floor_date) for details.
#' @param fn_summarize (optional) Function to recalculate to the specified time interval.
#' Default \code{fn_summarize ="mean"}, other examples are "median", "sum". See [dplyr::summarise](https://dplyr.tidyverse.org/reference/summarise.html) for details.
#' @return A dataframe with the performance metrics and ranking.
#' @importFrom dplyr mutate group_by summarize_all left_join select everything row_number %>%
#' @importFrom lubridate month floor_date
#' @importFrom purrr map_dbl map2 reduce
#' @importFrom stats cor
#' @importFrom readr parse_number
#' @importFrom hydroGOF NSE KGE pbias mae rsr
#' @export
#' @examples
#' \dontrun{
#' obj_tbl <- calculate_performance(sim = sim_flow, obs, "flo_day", c("kge", "nse"), "month", "sum")
#' }
#' @keywords calculate
#' @seealso \code{\link{calculate_performance_2plus}}

calculate_performance <- function(sim, obs, par_name = NULL, perf_metrics = NULL,
                                  period = NULL,
                                  fn_summarize = 'mean') {

  if(is.null(par_name)) {
    if(length(sim$simulation) > 1) {
      warning(paste0("You have multiple variable sets in the simulation object.\n
      They are ", paste(names(sim$simulation), collapse = ", "),
                     "\nCurrently, the first one is used, which is ", names(sim$simulation)[1],
                     ".\n If you want to use another one, please specify, which one you want to use with 'par_name' argument."))
    }
    # Filter to parameter set of interest.
    sim <- sim$simulation[[1]]
  } else {
    if(par_name %in% names(sim$simulation)){
      sim <- sim$simulation[[par_name]]
    } else {
      stop(paste0("The variable name '", par_name, "' does not exist in the simulation object!!!"))
    }
  }

  # Adding list of metric if not provided
  if(is.null(perf_metrics)) perf_metrics <- c("nse", "kge", "pbias", "r2", "mae", "rsr")

  # Initializing result table
  t <- data.frame(run_id = parse_number(names(sim[-1])))

  # Calculate the mean absolute error (MAE) for the average monthly values.
  if("mae" %in% perf_metrics){
    sim_m <- mutate(sim, date = month(date)) %>%
      group_by(date) %>%
      summarize_all(get(fn_summarize))
    obs_m <- mutate(obs, date = month(date)) %>%
      group_by(date) %>%
      summarize_all(get(fn_summarize))
    t$mae <- map_dbl(select(sim_m, - date), ~mae(.x, obs_m$value))
    t$rank_mae <- rank(abs(t$mae))

  }

  # Change the time step
  if(!is.null(period)) {
    sim <- mutate(sim, date = floor_date(date , period)) %>%
      group_by(date) %>%
      summarize_all(get(fn_summarize))
    obs <- mutate(obs, date = floor_date(date , period)) %>%
      group_by(date) %>%
      summarize_all(get(fn_summarize))
  }

  # Calculate the root square ratio
  if("rsr" %in% perf_metrics){
    # Calculate flow duration curves (FDC) for observed data and the simulations.
    fdc_obs <- calc_fdc(obs$value)
    fdc_sim <- calc_fdc(select(sim, -date))

    # Splitting intervals for the flow duration curve (FDC)
    p <- c(5, 20, 70, 95)
    p_lbl <- c('p_0_5', 'p_5_20', 'p_20_70', 'p_70_95', 'p_95_100')

    perf_metrics <- c(perf_metrics, gsub("p", "rsr", p_lbl))

    # Calculate the ratio of RSME and standard deviation for different segments
    # of the FDC (same as in the publications of the Kiel working group).
    rsr_fdc <- calc_fdc_rsr(fdc_sim, fdc_obs, p)


    fdc_thrs <- c(max(fdc_obs$value),
                  approx(fdc_obs$p, fdc_obs$value, p)$y,
                  -0.1)
    # Separate the hydrograph into high medium and low flows.
    obs_sep <- map2(fdc_thrs[1:(length(fdc_thrs) - 1)],
                    fdc_thrs[2:length(fdc_thrs)],
                    ~ mutate(obs, value = ifelse(value <= .x & value > .y, value, NA))) %>%
      map2(., p_lbl, ~ set_names(.x, c('date', .y))) %>%
      reduce(., left_join, by = 'date')

    t$rsr_vh <- -map_dbl(select(sim, - date), ~rsr(.x, obs_sep$p_0_5))
    t$rsr_h <- -map_dbl(select(sim, - date), ~rsr(.x, obs_sep$p_5_20))
    t$rsr_m <- -map_dbl(select(sim, - date), ~rsr(.x, obs_sep$p_20_70))
    t$rsr_l <- -map_dbl(select(sim, - date), ~rsr(.x, obs_sep$p_70_95))
    t$rsr_vl <- -map_dbl(select(sim, - date), ~rsr(.x, obs_sep$p_95_100))

    t$rsr_0_5 <- -rsr_fdc$p_0_5
    t$rsr_5_20 <- -rsr_fdc$p_5_20
    t$rsr_20_70 <- -rsr_fdc$p_20_70
    t$rsr_70_95 <- -rsr_fdc$p_70_95
    t$rsr_95_100 <- -rsr_fdc$p_95_100

    t$rank_rsr_0_5 <- rank(-t$rsr_0_5)
    t$rank_rsr_5_20 <- rank(-t$rsr_5_20)
    t$rank_rsr_20_70 <- rank(-t$rsr_20_70)
    t$rank_rsr_70_95 <- rank(-t$rsr_70_95)
    t$rank_rsr_95_100 <- rank(-t$rsr_95_100)
  }
  # Calculate the Nash-Sutcliffe efficiency (NSE)
  if ("nse" %in% perf_metrics){
    t$nse <- map_dbl(select(sim, - date), ~NSE(.x, obs$value))
    t$rank_nse <- rank(-t$nse)
  }
  # Calculate the Kling-Gupta efficiency (KGE)
  if("kge" %in% perf_metrics){
    t$kge <- map_dbl(select(sim, - date), ~KGE(.x, obs$value))
    t$rank_kge <- rank(-t$kge)
  }
  # Calculate the percent bias (PBIAS)
  if("pbias" %in% perf_metrics){
    t$pbias <- map_dbl(select(sim, - date), ~pbias(.x, obs$value))
    t$rank_pbias <- rank(abs(-abs(t$pbias)))
  }
  # Calculate the coefficient of determination (R2)
  if("r2" %in% perf_metrics){
    t$r2 <- map_dbl(select(sim, - date), ~cor(.x, obs$value)^2)
    t$rank_r2 <- rank(-t$r2)
  }

  # Calculate rank_tot.
  t <- t %>% mutate(rank_tot = as.integer(rank(rowSums(
    select(., starts_with(paste0('rank_', tolower(perf_metrics)))))))) %>%
    select(run_id, everything())

  return(t)
}


#' Calculate Performance Metrics for 2 of more variables
#'
#' This function calculates various performance metrics for two or more
#' variables between a given simulation and observed data.
#'
#' @param sim Object from SWATrunR
#' @param vector_var Character vector of variables to be used in performance
#' metric calculation. They should be present in sim object.
#' Example vector_var = c("flo_day_52", "no3_day_52_conc", "gwd_day").
#' The rest parameters should be provided in the same sequence as variables vector.
#' @param list_obs List of dataframes for the observed data with two columns:
#' 'date', 'value'.
#' @param list_periods (optional) List of two-element character vectors with the start and
#' end dates for the period to be used in the performance metric calculation for
#' each variable. Default \code{list_periods = NULL}, means full observation
#' data periods are used.
#' Example list_periods = list(c('2002-01-01', '2011-12-26'),
#' c('2007-01-01', '2008-12-26'), c('2007-01-01', '2011-12-26'))
#' @param vector_weights (optional) Numeric vector of weights for each variable in the
#' vector_var. Default \code{vector_weights = NULL}, which means all variables will
#' impact final results the same. vector_weights = c(0.5, 0.3, 0.2).
#' The sum of weights should be 1.
#' @param perf_metrics (optional) Character vector with the names of the performance metrics
#' to used in the rank_tot calculation. Default \code{perf_metrics = NULL},
#' which means that all (in this case c("nse", "kge", "pbias", "r2", "mae"))
#' performance metrics will be used in calculation. Other example could be
#' perf_metrics = c("kge", "nse"), which means that only KGE and NSE will be
#' used in calculation.

#' @return A list of two objects. One list 'obj_tbl_list' is storage with all performance
#' metrics for each variable. Another, 'obj_tbl' is dataframe with aggregated
#' results for all variables. This dataframe has final ranking and run ids/
#' @importFrom purrr pmap set_names map2 reduce
#' @importFrom dplyr mutate select across left_join
#' @export
#' @examples
#' \dontrun{
#' obj_tbl_m <- calculate_performance_2plus(sim,
#' vector_var = c("flo_day_52",  "no3_day_52_conc", "gwd_day"),
#' list_obs = list(obs_flow, obs_no3, obs_gwd)),
#' list_periods = list(c('2002-01-01', '2011-12-26'), NULL, c('2007-01-01', '2011-12-26')),
#' vector_weights = c(0.5, 0.3, 0.2),
#' perf_metrics = c("nse", "kge", "pbias", "r2", "mae")
#' }
#' @keywords calculate
#' @seealso \code{\link{calculate_performance}}

calculate_performance_2plus <- function(sim, vector_var, list_obs, list_periods = NULL,
                                     vector_weights = NULL,
                                     perf_metrics = NULL){

  ## Calculate performance data frames for each variable
  obj_tbl_list <- pmap(list(vector_var, list_obs, list_periods), function(.x, .y, .z){
    if(!is.null(.z)){
      tmp <- fix_dates(sim, .y, trim_start = .z[1], trim_end = .z[2])
    } else {
      tmp <- fix_dates(sim, .y, trim_start = min(.y$date), trim_end = max(.y$date))
    }
    calculate_performance(tmp$sim, tmp$obs, par_name = .x,
                          perf_metrics = ifelse(is.null(perf_metrics),
                                                c("nse", "kge", "pbias", "r2", "mae"),
                                                perf_metrics))}) %>%
    set_names(vector_var)

  # Calculate the mean of the performance rank for all parameter sets.
  obj_tbl <- map2(obj_tbl_list, ifelse(is.null(vector_weights), 1, vector_weights), function(.x, .y){
    select(.x, c(run_id, rank_tot)) %>%
      mutate(rank_tot = rank_tot * .y)}) %>%
    reduce(., left_join, by = 'run_id')%>%
    mutate(sum_rank = rowSums(across(starts_with("rank_tot")))) %>%
    mutate(rank_tot = as.integer(rank(sum_rank))) %>%
    select(run_id, rank_tot)

  return(list(obj_tbl_list = obj_tbl_list, obj_tbl = obj_tbl))
}
