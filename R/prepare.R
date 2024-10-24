# Calculate variables and parameters -------------------------------------------

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

#' Convert load and discharge time series into a concentration time series.
#'
#' Calculate a time series of concentration values from time series of
#' load and discharge.
#'
#' @param load Data frame with a date column and one or many columns with load
#'   values. The number of load columns must be the same as the number of
#'   discharge columns in the data frame `flow`.
#' @param flow Data frame with a date column and one or many columns with
#' discharge values. The number of flow columns must be the same as the number
#' of load columns in the data frame `load`.
#' @param load_unit Unit of the load values provided as a text string in the
#'   format `'mass time-1`. Default is `'kg day-1'`.
#' @param flow_unit Unit of the discharge values provided as a text string in
#' the format `'volume time-1`. Default is `'m3 s-1'`.
#' @param out_unit Unit of the concentration values provided as a text string in
#' the format `'mass volume-1`. Default is `'mg L-1'`.
#'
#' @importFrom dplyr bind_cols filter inner_join %>%
#' @importFrom lubridate is.Date
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect str_replace
#' @importFrom tibble as_tibble
#' @importFrom tidyselect any_of
#' @importFrom units as_units
#'
#' @return A table with date column and calculated concentration values.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' date_load <- seq(as.Date('2000-01-01'), as.Date('2000-12-31'), by = '2 week')
#' date_flow <- seq(as.Date('2000-01-01'), as.Date('2000-12-31'), by = 'day')
#'
#' l_df <- data.frame(date = date_load,
#'                    value = runif(length(date_load)))
#'
#' f_df <- data.frame(date = date_flow,
#'                    value = runif(length(date_flow)))
#'
#' conc <- load_to_conc(load = l_df, flow = f_df, load_unit = 'tons day-1')
#' }
#' @keywords data manipulation
#'
load_to_conc <- function(load, flow,
                         load_unit = 'kg day-1', flow_unit = 'm3 s-1',
                         out_unit = 'mg L-1') {
  # Get the names of the date columns
  date_col_load <- names(which(map_lgl(load, ~ is.Date(.x))))
  date_col_flow <- names(which(map_lgl(flow, ~ is.Date(.x))))

  if(length(date_col_load) == 0) {
    stop("No date column found in 'load'.")
  }
  if(length(date_col_load) > 1) {
    stop("More than one date columns found in 'load'.")
  }
  if(length(date_col_flow) == 0) {
    stop("No date column found in 'flow'.")
  }
  if(length(date_col_flow) > 1) {
    stop("More than one date columns found in 'flow'.")
  }

  names(load)[names(load) == date_col_load] <- 'date'
  names(flow)[names(load) == date_col_flow] <- 'date'

  # If unit column is in tables use unit and date as group/join variables
  if ('unit' %in% names(load) & 'unit' %in% names(flow)) {
    join_var <- c('unit', 'date')
  } else {
    join_var <- 'date'
  }

  # Check date columns for duplicate entries.
  if(anyDuplicated(load[, join_var])) {
    stop("Duplicated date entries were found in 'load'.")
  }
  if(anyDuplicated(flow[, join_var])) {
    stop("Duplicated date entries were found in 'flow'.")
  }

  # Generate a vector with dates which are available in load and flow
  dates_avail <- inner_join(load[join_var], flow[join_var], by = join_var)

  # Filter only dates where data in load and flow are available
  load <- filter(load, date %in% dates_avail$date)
  load <- select(load, - any_of(join_var))
  flow <- filter(flow, date %in% dates_avail$date)
  flow <- select(flow, - any_of(join_var))

  # Add 1000 if unit is tons. This workaround was introduced as
  # metric tons in package units are not exactly 1000 kg.
  if(str_detect(load_unit, 'tons')) {
    # install_unit('tons', '1000 kg', 'Metric tons')
    c1 <- 1000
    load_unit <- str_replace(load_unit, 'tons', 'kg')
  } else {
    c1 <- 1
  }

  # Get conversion factor from units
  l <- as_units(1, load_unit)
  f <- as_units(1, flow_unit)
  c <- c1*l/f
  units(c) <- out_unit
  c <- as.numeric(c)

  # Calculate concentration tibble from load flow and conversion factor
  conc <- as_tibble(c* as.matrix(load) / as.matrix(flow))
  conc <- bind_cols(dates_avail, conc)

  return(conc)
}

#' Convert concentration and discharge time series into a load time series.
#'
#' Calculate a time series of load values from time series of
#' concentration and discharge.
#'
#' @param conc Data frame with a date column and one or many columns with
#' concentration values. The number of concentration columns must be the same as
#' the number of discharge columns in the data frame `flow`.
#' @param flow Data frame with a date column and one or many columns with
#' discharge values. The number of flow columns must be the same as the number
#' of concentration columns in the data frame `conc`.
#' @param conc_unit Unit of the concentration values provided as a text string in
#' the format `'mass volume-1`. Default is `'mg L-1'`.
#' @param flow_unit Unit of the discharge values provided as a text string in
#' the format `'volume time-1`. Default is `'m3 s-1'`.
#' @param out_unit Unit of the load values provided as a text string in the
#'   format `'mass time-1`. Default is `'kg day-1'`.
#'
#' @importFrom dplyr bind_cols filter inner_join %>%
#' @importFrom lubridate is.Date
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect str_replace
#' @importFrom tibble as_tibble
#' @importFrom tidyselect any_of
#' @importFrom units as_units
#'
#' @return A table with date column and calculated concentration values.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' date_conc <- seq(as.Date('2000-01-01'), as.Date('2000-12-31'), by = '2 week')
#' date_flow <- seq(as.Date('2000-01-01'), as.Date('2000-12-31'), by = 'day')
#'
#' c_df <- data.frame(date = date_conc,
#'                    value = runif(length(date_conc)))
#'
#' f_df <- data.frame(date = date_flow,
#'                    value = runif(length(date_flow)))
#'
#' load <- conc_to_load(conc = c_df, flow = f_df, out_unit = 'tons day-1')
#' }
#' @keywords data manipulation
#'
conc_to_load <- function(conc, flow,
                         conc_unit = 'mg L-1', flow_unit = 'm3 s-1',
                         out_unit = 'kg day-1') {
  # Get the names of the date columns
  date_col_conc <- names(which(map_lgl(conc, ~ is.Date(.x))))
  date_col_flow <- names(which(map_lgl(flow, ~ is.Date(.x))))

  if(length(date_col_conc) == 0) {
    stop("No date column found in 'conc'.")
  }
  if(length(date_col_conc) > 1) {
    stop("More than one date columns found in 'conc'.")
  }
  if(length(date_col_flow) == 0) {
    stop("No date column found in 'flow'.")
  }
  if(length(date_col_flow) > 1) {
    stop("More than one date columns found in 'flow'.")
  }

  names(conc)[names(conc) == date_col_conc] <- 'date'
  names(flow)[names(flow) == date_col_flow] <- 'date'

  # If unit column is in tables use unit and date as group/join variables
  if ('unit' %in% names(conc) & 'unit' %in% names(flow)) {
    join_var <- c('unit', 'date')
  } else {
    join_var <- 'date'
  }

  # Check date columns for duplicate entries.
  if(anyDuplicated(conc[, join_var])) {
    stop("Duplicated date entries were found in 'conc'.")
  }
  if(anyDuplicated(conc[, join_var])) {
    stop("Duplicated date entries were found in 'flow'.")
  }

  # Generate a vector with dates which are available in load and flow
  dates_avail <- inner_join(conc[join_var], flow[join_var], by = join_var)

  # Filter only dates where data in conc and flow are available
  conc <- filter(conc, date %in% dates_avail$date)
  conc <- select(conc, - any_of(join_var))
  flow <- filter(flow, date %in% dates_avail$date)
  flow <- select(flow, - any_of(join_var))

  # Add 1000 if unit is tons. This workaround was introduced as
  # metric tons in package units are not exactly 1000 kg.
  if(str_detect(out_unit, 'tons')) {
    # install_unit('tons', '1000 kg', 'Metric tons')
    c1 <- 1000
    out_unit <- str_replace(out_unit, 'tons', 'kg')
  } else {
    c1 <- 1
  }

  # Get conversion factor from units
  c <- as_units(1, conc_unit)
  f <- as_units(1, flow_unit)
  l <- c*f/c1
  units(l) <- out_unit
  l <- as.numeric(l)

  # Calculate concentration tibble from load flow and conversion factor
  load <- as_tibble(l* as.matrix(conc) * as.matrix(flow))
  load <- bind_cols(dates_avail, load)

  return(load)
}

# Generating parameter samples  ---------------------------------------------------------------------

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

#' Sample OAT Function
#'
#' Generates a set of OAT (One-At-a-Time) samples based on provided parameter centers and boundaries.
#'
#' @param par A matrix or data frame specifying the parameter ranges.
#' Each element is length 2 indicating the min and max values.
#' @param par_center (optional) A data frame or matrix of parameter centers.
#' Default \code{par_center = 1}.
#' @param n_t (optional) An integer specifying the parameter combination around,
#'  which you want to do the OAT analysis. Default \code{n_t = 10}.
#' @return A data frame containing the OAT samples.
#' @importFrom purrr map map_df
#' @export
#' @examples
#' \dontrun{
#' # Define parameter ranges
#' par <- data.frame("snomelt_tmp.hru | change = absval" = c(-1, 1),
#'                  "snofall_tmp.hru | change = absval" = c(-1, 1))
#' par_center <- 2
#' par_oat <- sample_oat(par, par_center)
#' }
#' @keywords sampling
#' @seealso \code{\link{plot_oat}}

sample_oat <- function(par, par_center = 1, n_t = 10) {
  n_c <- nrow(par_center)

  par_transect <- map(par, ~ seq(.x[1], .x[2], length.out = n_t))

  par_oat <- map_df(1:n_c, ~ sample_transect_i(par, par_center, par_transect, .x))

  return(par_oat)
}

#' Group the values of a parameter in the file hydrology.hyd.
#'
#' This is useful for applying different parameter ranges e.g. to parameters
#' such as perco, cn3_swf, or latq_co which have different initial values
#' based on runoff or leaching potential.
#'
#' @param par_name Name of the parameter for which values should be grouped
#' @param model_path path to the SWAT+ model project folder which contains the
#'   file hydrology.hyd
#'
#' @returns A vector with unique indices of the parameter value groups assigned
#'   to each HRU.
#' @export
#'
group_hydr_values <- function(par_name, model_path) {
  hyd_hyd <- read_tbl(paste0(model_path, '/hydrology.hyd'))

  unique_values <- sort(unique(hyd_hyd[[par_name]]))
  value_group <- map_int(hyd_hyd[[par_name]], ~ which(.x == unique_values))
  unique_values <- paste(paste0(1:length(unique_values), ' = ', unique_values), collapse = ', ')

  comment(value_group) <- unique_values

  cat('Following unique initial', par_name, 'values were identified',
      'and grouped:', '\n')
  cat(unique_values, '\n')

  return(value_group)
}



#' Translate the normalized sampled values of a parameter to ranges of groups
#' of that parameter (e.g. when perco should have different ranges based on the
#' initial values of perco in all HRUs)
#'
#' @param par_tbl Table with the sampled parameter combinations
#' @param par_name Name of the parameter which should be translated
#' @param par_bound List of parameter boundaries to which the normalized values
#'   should be transformed to.
#' @param par_group (named) Integer vector which defines the groups to which each
#'   spatial unit belongs to.
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr map map2_lgl set_names
#' @importFrom stringr str_detect str_extract str_remove str_split str_trim
#' @importFrom tibble add_column
#'
#' @returns The table with the parameter samples, where the normalized parameter
#'   is replaced by several columns with the updated parameter boundaries and
#'   correct names assigned, so that the parameter boundaries are correctly applied.
#' @export
#'
translate_to_boundaries <- function(par_tbl, par_name,  par_bound, par_group) {
  stopifnot(length(par_bound) == length(unique(par_group)))

  if (is.null(names(par_bound))) {
    names(par_bound) <- as.character(1:length(par_bound))
  }

  attr <- attributes(par_group)
  if(!is.null(attr)) {
    par_init_vals <- attr %>%
      str_split(., ', ', simplify = T) %>%
      str_remove(., '[0-9]+ = ') %>%
      as.numeric(.)

    is_in_bound <- map2_lgl(par_init_vals, par_bound, ~ .x >= .y[1] & .x <= .y[2])

    if(any(!is_in_bound)) {
      stop("The following boundaries in 'par_bound' do not cover their initial values:",
           paste(names(par_bound)[!is_in_bound], collapse = ', '))
    }
  }

  par_pos <- which(names(par_tbl) == par_name)

  if(length(par_pos) == 0) {
    stop("Parameter '", par_name, "' not found in 'par_tbl'.")
  }

  if (str_detect(par_name, 'unit')) {
    stop("'par_name' has already a 'unit' condition assigned!")
  }

  norm_vals <- par_tbl[[par_name]]

  if(any(norm_vals < 0) | any(norm_vals > 1)) {
    stop("Values of parameter '", par_name, "' must be in the range 0 to 1.")
  }

  if (str_detect(par_name, '::')) {
    par_rest <- str_trim(str_extract(par_name, '::.*'))
    par_lbl <- str_trim(str_remove(par_name, '::.*'))
  } else {
    par_lbl <- str_trim(str_remove(par_name, '\\..*'))
    par_rest <- paste0('::', par_name)
  }

  unit_str <- map(sort(unique(par_group)), ~ which(par_group == .x)) %>%
    map(build_unit_string)

  par_tbl_add <- map(par_bound, ~ norm_vals * (max(.x) - min(.x)) + min(.x)) %>%
    bind_cols(.) %>%
    set_names(paste0(par_lbl, '_', names(.), par_rest, unit_str))

  par_tbl <- par_tbl[,-par_pos]
  par_tbl <- add_column(par_tbl, par_tbl_add, .before = par_pos)

  return(par_tbl)
}

#' Build the text string with the 'unit' definition to be added to a parameter
#' name
#'
#' @param vals Vector of unit IDs
#'
#' @importFrom purrr map2_chr
#'
#' @returns A text string with the unit definition.
#'
#' @keywords internal
#'
build_unit_string <- function(vals) {
  vals <- sort(vals)
  diff_vals <- diff(vals)

  end_seq   <- unique(c(vals[diff_vals != 1], vals[length(vals)]))
  start_seq <- unique(c(vals[1], vals[which(diff_vals != 1) + 1]))

  map2_chr(start_seq, end_seq, ~paste_runs(.x, .y, sep = ':')) %>%
    paste(., collapse = ', ') %>%
    paste0(' | unit = c(', ., ')')
}

#' Paste run indexes if start and end of sequence differ. Otherwise only use
#' start value
#'
#' @param strt Numeric start value of sequence
#' @param end  Numeric end value of sequence
#'
#' @keywords internal
#'
paste_runs <- function(strt, end, sep) {
  if(strt == end) {
    as.character(strt)
  } else {
    paste(strt, end, sep = sep)
  }
}


# Evaluate model performance ---------------------------------------------------

#' Calculate a goodness-of-fit table
#'
#' `calc_gof()` calculates goodness-of-fit inices for simulated time series
#' `sim` and observations `obs` applying the functions passed as a list
#' with `funs`.
#'
#' @param sim Data frame with one date column and one or many columns with
#'   values of the simulated variable.
#' @param obs Data frame with one date and one value column.
#' @param funs List of functions which are applied to calculate the
#'   goodnes-of-fit values. The list can be a named list
#' (e.g. `list(nse_q = NSE, pb_q = pbias)`). In this case the column names of
#' returned table will be those names. If the list is unnamed then the function
#' names will be the column names.
#'
#' @returns A table with the calculated goodness-of-fit values.
#'
#' @importFrom dplyr bind_cols filter inner_join mutate select %>%
#' @importFrom lubridate is.Date
#' @importFrom purrr map_lgl
#' @importFrom tidyselect any_of
#'
#' @export
#'
#' @examples
#' \dontrun{
#' date_flow <- seq(as.Date('2000-01-01'), as.Date('2000-12-31'), by = 'day')
#'
#' flow_sim <- data.frame(date = date_flow,
#'                        run_1 = runif(length(date_flow)),
#'                        run_2 = runif(length(date_flow))
#'                        )
#' flow_obs <- data.frame(date = date_flow,
#'                        qobs = runif(length(date_flow))
#'                        )
#'
#' obj_tbl <- calc_gof(sim = flow_sim, obs = flow_obs, funs = list(cor_q = cor))
#' }
#'
#' @keywords calculate
#'
calc_gof <- function(sim, obs, funs) {
  fun_names <- as.character(substitute(funs))[-1]
  list_names <- names(funs)
  fun_names[nchar(list_names) > 0] <- list_names[nchar(list_names) > 0]

  if(ncol(obs) != 2) {
    stop("'obs' must consist of one date and one variable column.")
  }

  # Get the names of the date columns
  date_col_sim <- names(which(map_lgl(sim, ~ is.Date(.x))))
  date_col_obs <- names(which(map_lgl(obs, ~ is.Date(.x))))

  if(length(date_col_sim) == 0) {
    stop("No date column found in 'sim'.")
  }
  if(length(date_col_sim) > 1) {
    stop("More than one date columns found in 'sim'.")
  }
  if(length(date_col_obs) == 0) {
    stop("No date column found in 'obs'.")
  }
  if(length(date_col_obs) > 1) {
    stop("More than one date columns found in 'obs'.")
  }

  names(sim)[names(sim) == date_col_sim] <- 'date'
  names(obs)[names(obs) == date_col_obs] <- 'date'

  # If unit column is in tables use unit and date as group/join variables
  if ('unit' %in% names(sim) & 'unit' %in% names(obs)) {
    join_var <- c('unit', 'date')
  } else {
    join_var <- 'date'
  }

  # Check date columns for duplicate entries.
  if(anyDuplicated(sim[, join_var])) {
    stop("Duplicated date entries were found in 'load'.")
  }
  if(anyDuplicated(sim[, join_var])) {
    stop("Duplicated date entries were found in 'flow'.")
  }

  # Generate a vector with dates which are available in load and flow
  dates_avail <- inner_join(sim[join_var], obs[join_var], by = join_var)

  # Filter only dates where data in load and flow are available
  sim <- filter(sim, date %in% dates_avail$date)
  sim <- select(sim, - any_of(join_var))
  obs <- filter(obs, date %in% dates_avail$date)
  obs <- select(obs, - any_of(join_var))

  gofs <- map(funs, ~ calc_gof_i(sim, obs, .x)) %>%
    bind_cols(., .name_repair = ~ fun_names) %>%
    mutate(., run = names(sim), .before = 1)

  return(gofs)
}

#' Calculate goodness-of-fit values for one function
#'
#' @param sim Data frame with one or many columns with values of the simulated
#'   variable.
#' @param obs Data frame with one value column.
#' @param fun Goodness-of-fit function
#'
#' @importFrom purrr map_dbl
#'
#' @returns A vector of goodness-of-fit values for each column of `sim`
#'   calculated with `fun`
#' @export
#'
#' @keywords internal
#'
calc_gof_i <- function(sim, obs, fun) {
  map_dbl(sim, ~ fun(.x, obs[[1]]))
}

#' Rank lines in a table of goodness-of-fit values.
#'
#' `rank_gof()` ranks each column of a goodness-of-fit table either by absolute
#' ranks or normalized GOF values and calculates a total rank for each line.
#'
#' @param gof_tbl Table with goodness-of-fit values, where each column provides
#'   the values for all `run`s of one GOF index
#' @param weights (optional) weights vector to apply different weights to
#'   the goodness-of-fit indices for the calculation of the total rank.
#' @param type Either `'rank'` to calculate absolute ranks or `'norm'` to use
#'   normalized values of the indices.
#'
#' @importFrom dplyr arrange bind_cols mutate select %>%
#' @importFrom purrr map map2 pmap_dbl
#' @importFrom tidyselect any_of
#'
#' @returns A rank table for all goodness-of-fit columns and a total rank.
#'
#' @export
#'
rank_gof <- function(gof_tbl, weights = NULL, type = 'rank') {
  runs <- select(gof_tbl, any_of('run'))
  gof_tbl <- select(gof_tbl, -any_of('run'))

  if(type == 'rank') {
    rank_tbl <- gof_tbl %>%
      map(., rank) %>%
      bind_cols(.)
  } else {
    rank_tbl <- gof_tbl %>%
      map(., normalize) %>%
      bind_cols(.)
  }
  if(! is.null(weights)) {
    if(length(weights) != ncol(gof_tbl)) {
      stop("'weights' must be the same length as the number of columns of 'gof_tbl'.")
    }
    weights <- weights / sum(weights)
  } else {
    weights <- rep(1, ncol(gof_tbl)) / ncol(gof_tbl)
  }

  rank_tot <- map2(rank_tbl, weights, ~ .x*.y) %>%
    pmap_dbl(., sum)

  if(type == 'rank') {
    rank_tot <- round(rank_tot)
  }

  rank_tbl <- rank_tbl %>%
    mutate(run = runs[[1]],
           rank_tot = rank_tot,
           .before = 1) %>%
    arrange(., rank_tot)

  return(rank_tbl)
}

#' Normalize values in vector between 0 and 1
#'
#' @param x Numeric vector
#'
#' @returns A vector with normalized values.
#'
#' @keywords internal
#'
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
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
#'   vector_var = c("flo_day_52",  "no3_day_52_conc", "gwd_day"),
#'   list_obs = list(obs_flow, obs_no3, obs_gwd),
#'   list_periods = list(c('2002-01-01', '2011-12-26'),
#'     NULL, c('2007-01-01', '2011-12-26')),
#'   vector_weights = c(0.5, 0.3, 0.2),
#'   perf_metrics = c("nse", "kge", "pbias", "r2", "mae"))
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
