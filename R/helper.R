#' Calculate water yield ratio
#'
#' Calculate the water yield ratio from simulated average annual water balance
#' components. The simulation run must provide
#' \describe{
#'     \item{precip}{Variable out of 'basin_wb_aa' output file.}
#'     \item{surq_cha}{Variable out of 'basin_wb_aa' output file.}
#'     \item{surq_res}{Variable out of 'basin_wb_aa' output file.}
#'     \item{latq_cha}{Variable out of 'basin_wb_aa' output file.}
#'     \item{latq_res}{Variable out of 'basin_wb_aa' output file.}
#'     \item{qtile}{Variable out of 'basin_wb_aa' output file.}
#'     \item{flo}{Variable out of ''basin_aqu_aa' output file.}
#'   }
#'
#' @param sim A SWATrunR simulation run with the above listed simulated
#'   variables.
#'
#' @returns The water yield ratio.
#' @export
#'
calc_wyr <- function(sim) {
  unname((sim$simulation$flo +
          sim$simulation$surq_cha +
          sim$simulation$surq_res +
          sim$simulation$latq_cha +
          sim$simulation$latq_res +
          sim$simulation$qtile) /
          sim$simulation$precip)
}

# Different parameter values for groupings -------------------------------------
#' ID Grouping Functions for Parameter Names
#'
#' This function groups values for parameter names, sorting and concatenating
#' numeric sequences while handling non-numeric values appropriately.
#'
#' @param vals A vector of values to be grouped. Can be numeric or character.
#' @param sep (optional) A string separator to use between ranges of numeric
#' values. Default \code{sep = ':'}.
#' @param n_trunc (optional) An integer specifying the maximum number of elements
#' to include in the output. Default \code{sep = Inf}, which means no truncation.
#' @importFrom purrr map2_chr
#'
#' @return A string with grouped values. For numeric inputs, ranges are collapsed
#' and separated by the specified separator. Non-numeric inputs are concatenated
#' with ', '. If all values are NA, an empty string is returned.
#'
#' @keywords internal
#'
group_values <- function(vals, sep = ':', n_trunc = Inf) {
  if(all(is.na(vals))) {
    ''
  } else if (is.numeric(vals[1])) {
    vals <- sort(vals)
    diff_vals <- diff(vals)

    end_seq   <- unique(c(vals[diff_vals != 1], vals[length(vals)]))
    start_seq <- unique(c(vals[1], vals[which(diff_vals != 1) + 1]))

    map2_chr(start_seq, end_seq, ~paste_runs(.x, .y, sep = sep)) %>%
      truncate(., n_trunc, side = 'both')
  } else {
    paste(vals, collapse = ', ')
  }
}

#' Combine Start and End Values into a Range String
#'
#' This function creates a string representation of a range by combining start
#' and end values. If the start and end values are the same, it returns the
#' single value as a string. Otherwise, it concatenates them with a separator.
#'
#' @param strt The starting value of the range.
#' @param end The ending value of the range.
#' @param sep A string separator to use between the start and end values. Default is ':'.
#'
#' @return A string representing the range. If the start and end values are the same,
#' the single value is returned. Otherwise, the start and end values are concatenated
#' with the specified separator.

#' @keywords internal

paste_runs <- function(strt, end, sep) {
  if(strt == end) {
    as.character(strt)
  } else {
    paste(strt, end, sep = sep)
  }
}

#' Truncate a Vector with Ellipses
#'
#' This function truncates a vector and adds ellipses (`"..."`) to indicate omitted values.
#' Truncation can be done from the left or both sides.
#'
#' @param x A vector to be truncated.
#' @param n An integer specifying the maximum number of elements to include before truncation.
#' @param side A string indicating the side from which to truncate. Options are `'left'` or `'both'`.
#' Default is `'left'`.
#'
#' @return A string with the truncated vector. If truncation occurs, ellipses (`"..."`) are added
#' to indicate omitted values.
#' @keywords internal

truncate <- function(x, n, side = 'left') {
  if (side == 'left') {
    if(!is.na(x[n])) {
      x <- c(x[1:n],"...")
    }
  } else if (side == 'both') {
    if(length(x) > (n + 1)) {
      x <- c(x[1:(n/2)],"...", x[(length(x) - (n/2)) : length(x)])
    }
  }

  paste(x, collapse = ", ")
}


#' Generate 'unit' condition for parameter definition from ID vector
#'
#' `id_to_unit()` generates a 'unit' condition for  `SWATrunR` parameter
#' definitions from an ID vector. This is useful when e.g. a vector of HRU IDs
#' is given and a parameter change change should be defined for those units
#'
#' @param ids Numeric ID vector.
#'
#' @returns Character string with unit condition based on the given IDs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # HRU IDs e.g. same land use
#' hru_ids <- c(1, 3, 4, 5, 28, 37:43, 58)
#'
#' # E.g. define a change of cn2
#' cn2_chg <- c('cn2_agri::cn2.hru | change = abschg' = - 5)
#'
#' # Add unit condition
#' names(cn2_chg) <- paste(names(cn2_chg), id_to_unit(hru_ids))
#' }

id_to_unit <- function(ids){
  paste0('| unit = c(', group_values(ids), ')')
}

# Performance calculation ------------------------------------------------------
#' Filter a time period for a variable time series table.
#'
#' `filter_period()` filters rows from a table which has one date column, where
#' the dates are in the time range within the `time_window`.
#'
#' @param tbl Variable time series table, were one column is of type date and
#'   all other columns are numeric values
#' @param time_window Vector of length 2 which provides the start and end dates
#'   of the time window to be filtered. Must be in any format such as
#'   `c(2000, 2010)`, or `c(20000101, 20101231)`, or e.g.
#'   `c('2000-01-01', '2010-12-31')`
#'
#' @returns The table `tbl` with the filtered rows based on the `time_window`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flow_sim <- filter_period(tbl = flow_sim, time_window = c(2010, 2015))
#' }
#' @keywords data manipulation
filter_period <- function(tbl, time_window) {
  date_col <- names(which(map_lgl(tbl, is.Date)))

  if(length(date_col) != 1) {
    stop("Exactly one column in 'tbl' must be of type 'Date'.")
  }

  if(length(time_window) != 2) {
    stop("'time_window' must be of length 2 providing a start and an end date.")
  }
  if(nchar(time_window[1]) != nchar(time_window[1])) {
    stop("The formats of start and end date of 'time_window' differ.")
  }
  if(nchar(time_window[1]) == 4) {
    time_window[1] <- paste0(as.character(time_window[1]), '-01-01')
    time_window[2] <- paste0(as.character(time_window[2]), '-12-31')
  }

  time_window <- as.Date(time_window)

  if (time_window[1] > time_window[2]) {
    stop("The first value of 'time_window' is greater than the second value.")
  }

  tbl <- filter(tbl,
                !!sym(date_col) >= time_window[1],
                !!sym(date_col) <= time_window[2])

  return(tbl)
}

# Flow duration curve calculation functions ------------------------------------

#' Calculate Flow Duration Curve (FDC)
#'
#' This function calculates the flow duration curve for a given vector or dataframe.
#'
#' @param x a vector or a tibble with flow values.
#'
#' @return a tibble with sorted values and their corresponding exceedance
#' probabilities.
#'
#' @importFrom dplyr group_split mutate select %>%
#' @importFrom purrr list_rbind map map2
#' @importFrom tibble tibble
#' @importFrom tidyselect any_of
#'
#' @examples
#' \dontrun{
#' fdc <- calc_fdc(c(3, 1, 4, 1, 5, 9, 2, 6, 5))
#' }
#'
#' @export
#' @keywords helper
#'
calc_fdc <- function(x) {
  if(is.vector(x)) {
    x <- tibble(value = x)
  }
  x <- select(x, - any_of('date'))

  if('unit' %in% names(x)) {
    u_i <- unique(x$unit)
    x <- x %>%
      mutate(unit = factor(unit, u_i)) %>%
      group_split(., unit) %>%
      map(., ~ select(.x, -unit))
    x <- x %>%
      map(., calc_fdc_i) %>%
      map2(., u_i, ~ mutate(.x, unit = .y, .before = 1)) %>%
      list_rbind(.)

  } else {
    x <- calc_fdc_i(x)
  }

  return(x)
}

#' Calculate Flow Duration Curve (FDC) for list element i
#'
#' Calculate the flow duration curve for a given vector or dataframe.
#'
#' @param x A vector or a tibble with flow values.
#'
#' @return a tibble with sorted values and their corresponding exceedance
#' probabilities.
#'
#' @importFrom dplyr mutate %>%
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
calc_fdc_i <- function(x) {
  n <- nrow(x)

  x %>%
    apply(., 2, sort, decreasing = TRUE) %>%
    as_tibble(.) %>%
    mutate(p = 100 * 1:n / (n + 1), .before = 1)
}

#' Calculate RSR for Flow Duration Curve Segments
#'
#' This function calculates the ratio of RMSE and standard deviation for
#' different segments of the flow duration curve (FDC).
#'
#' @param fdc_sim a tibble with simulated flow data.
#' @param fdc_obs a tibble with observed flow data.
#' @param quantile_splits a numeric vector with quantiles for splitting the FDC.
#' @param out_tbl character specifying the output format ('long' or 'wide').
#' Default \code{out_tbl = 'long'}.
#'
#' @return a tibble with RSR values for the different segments of the FDC.
#'
#' @importFrom dplyr select mutate bind_cols bind_rows %>%
#' @importFrom purrr map2 map_dbl
#'
#' @examples
#' \dontrun{
#' fdc_sim <- calc_fdc(runif(100))
#' fdc_obs <- calc_fdc(runif(100))
#' rsr_values <- calc_fdc_rsr(fdc_sim, fdc_obs, c(5, 20, 70, 95))
#' }
#'
#' @export
#' @keywords helper
#'
calc_fdc_rsr <- function(fdc_sim, fdc_obs, quantile_splits, out_tbl = 'long') {
  if(all(quantile_splits <= 1)) {
    quantile_splits <- 100 * quantile_splits
  }
  quantile_splits <- sort(unique(c(0, 100, quantile_splits)))
  p_cuts <- cut(fdc_obs$p, quantile_splits)
  obs <- split(select(fdc_obs, -p), p_cuts)
  sim <- split(select(fdc_sim, -p), p_cuts)

  rsr_list <- map2(sim, obs, ~ rsr_df(.x, .y[[1]]))

  if(out_tbl == 'long') {
    n_col <- length(quantile_splits) - 1
    col_names <- paste0('p_', quantile_splits[1:n_col],
                        '_',  quantile_splits[2:(n_col + 1)])
    rsr <- bind_cols(rsr_list) %>%
      set_names(col_names) %>%
      mutate(., run = names(fdc_sim)[2:ncol(fdc_sim)], .before = 1)
  } else {
    rsr <- rsr_list %>%
      bind_rows(.) %>%
      mutate(p = unique(p_cuts), .before = 1)
  }
  return(rsr)
}

#' Calculate RSR for Dataframe Segments
#'
#' This function calculates the RSR values for the different segments of the data.
#'
#' @param df_sim a dataframe with simulated values.
#' @param v_obs a dataframe with observed values.
#'
#' @return a numeric vector with RSR values for each segment.
#'
#' @importFrom purrr map_dbl
#' @importFrom hydroGOF rsr
#'
#' @examples
#' \dontrun{
#' rsr_values <- rsr_df(df_sim, v_obs)
#' }
#' @keywords internal

rsr_df <- function(df_sim, v_obs) {
  map_dbl(df_sim, ~ rsr(.x, v_obs))
}

# Plot parameter identifiability -----------------------------------------------

#' Calculate Segment Differences
#'
#' This function calculates segment differences for a given set of parameters.
#'
#' @param par A list or data frame of parameters to segment.
#' @param obj A numeric vector of objective values.
#' @param obj_thrs A numeric threshold for the objective values.
#'
#' @return A data frame containing the calculated segment differences.
#'
#' @importFrom dplyr bind_cols group_by summarise mutate ungroup %>%
#' @importFrom purrr map map2_df
#'
#' @keywords internal

calc_segment_diff <- function(par, obj, obj_thrs) {

  # Cut the parameters into 20 segments
  par_cuts <- map_df(par, ~ cut(.x, 20))

  # Calculate the fill_segment for each parameter segment
  tbl <- map(par_cuts, ~ bind_cols(segment = .x, obj = obj)) %>%
    map(., ~ mutate(.x, obj = obj >= obj_thrs)) %>%
    map(., ~ group_by(.x, segment)) %>%
    map(., ~ summarise(.x, fill_segment = sum(obj))) %>%
    map2_df(., names(.), ~ mutate(.x, parameter = .y, .before = 1)) %>%
    group_by(parameter) %>%
    mutate(., fill_segment = 100 * (fill_segment/sum(fill_segment) - 0.05) / 0.05) %>%
    ungroup()

  # Convert segment cuts to rectangle coordinates
  x_segment <- cuts_to_rect(tbl$segment)
  tbl <- bind_cols(tbl, x_segment)

  return(tbl)
}

#' Convert Cuts to Rectangle Coordinates
#'
#' This function converts cut labels into rectangle coordinates.
#'
#' @param lbl A character vector of cut labels.
#'
#' @importFrom dplyr tibble %>%
#' @importFrom stringr str_remove_all str_split
#' @importFrom purrr map map_df
#'
#' @return A data frame containing the rectangle coordinates.
#'
#' @keywords internal

cuts_to_rect <- function(lbl) {
  lbl %>%
    as.character() %>%
    str_remove_all(., '\\(|\\]') %>%
    str_split(., ',') %>%
    map(., ~ as.numeric(.x)) %>%
    map_df(., ~ tibble(xmin = .x[1], xmax = .x[2]))
}

# OAT analysis -----------------------------------------------------------------

#' Sample Transect for a Specific Parameter
#'
#' Generates samples along the transect for a specific parameter.
#'
#' @param par_bnd A list of parameter boundaries.
#' @param par_center A data frame or matrix of parameter centers.
#' @param par_transect A list of parameter transects.
#' @param i_c An integer indicating the index of the current parameter center.
#' @return A data frame of samples along the transect for the specified parameter.
#' @importFrom dplyr bind_rows distinct mutate relocate %>%
#' @importFrom purrr map map2
#' @keywords internal

sample_transect_i <- function(par_bnd, par_center, par_transect, i_c) {
  n_t <- length(par_transect[[1]])
  c_i    <- par_center[i_c,]
  c_i_nt <- c_i[rep(1,n_t),]
  c_i_add <- mutate(c_i, center = i_c)
  par_transect %>%
    map2(., c_i, ~ replace_closest_value(.x, .y)) %>%
    map2(., 1:ncol(c_i), ~ replace_col_by_sequence(c_i_nt, .x, .y)) %>%
    map(., ~ mutate(.x, center = i_c)) %>%
    bind_rows(.) %>%
    bind_rows(c_i_add, .) %>%
    distinct() %>%
    mutate(., parameter = c('center', rep(names(c_i), each = n_t - 1))) %>%
    relocate(., center, parameter, .before = 1)
}

#' Replace Closest Value in Sequence
#'
#' Replaces the closest value in a sequence with a specified value.
#'
#' @param val_seq A numeric vector representing a sequence of values.
#' @param x A numeric value to find the closest match in the sequence.
#' @return A numeric vector with the closest value replaced by \code{x}.
#' @keywords internal

replace_closest_value <- function(val_seq, x) {
  i_mdist <- which.min(abs(val_seq - x))
  val_seq[i_mdist] <- x
  return(val_seq)
}

#' Replace Column by Sequence
#'
#' Replaces the specified column in a table with a sequence of values.
#'
#' @param tbl A data frame or matrix.
#' @param val_seq A numeric vector representing a sequence of values.
#' @param i An integer indicating the index of the column to be replaced.
#' @return The modified data frame or matrix with the specified column replaced
#' by \code{val_seq}.
#' @keywords internal

replace_col_by_sequence <- function(tbl, val_seq, i) {
  tbl[i] <- val_seq
  return(tbl)
}

# All other --------------------------------------------------------------------

#' Remove Unsuccessful Runs from Simulation
#'
#' This function removes unsuccessful runs from the simulation parameter set
#' to ensure that all analyses are only performed for parameter combinations
#' that have corresponding simulation results.
#'
#' @param sim A SWATrunR nested list containing simulation data. : `simulation` and `parameter`.
#'   - `simulation`: A list where the first element contains the results of the simulation runs.
#'   - `parameter`: A list with an element `values` which is a data frame containing the parameter sets used in the simulation.
#'
#' @return A modified version of the input `sim` list, with the `parameter$values`
#' element containing only the parameter sets for which simulation results are available.
#' @importFrom dplyr %>%
#' @importFrom stringr str_remove
#' @export
#' @examples
#' \dontrun{
#' sim_flow_full <- remove_unsuccesful_runs(sim_flow_full_bck)
#' }
#' @keywords helper

remove_unsuccesful_runs <- function(sim){
  id_runs <- sim$simulation[[1]] %>%
    names(.) %>%
    .[2:length(.)] %>%
    str_remove(., 'run_') %>%
    as.numeric(.)

  sim$parameter$values <- sim$parameter$values[id_runs,]
  rownames(sim$parameter$values) <- id_runs

  return(sim)
}


#' Add a running ID to duplicated names
#'
#' @param col_name Character vector of column names
#' @returns the `col_name` character vector with IDs for duplicated names
#' @keywords internal

add_suffix_to_duplicate <- function(col_name){
  dupl <- table(col_name) %>%
    .[. > 1]

  if(length(dupl > 0)) {
    for(i in 1:length(dupl)) {
      col_name[col_name == names(dupl[i])] <-
        paste0(names(dupl[i]), c('', 1:(dupl[i]-1)))
    }
  }

  return(col_name)
}

#' Copy a file with an optional version suffix.
#'
#' This function copies a file from one location to another, with an optional
#' version suffix appended to the filename.
#'
#' @param model_path The path to the directory containing the file.
#' @param file_name The name of the file to be copied.
#' @param file_version An optional version suffix to be appended to the filename.
#' Default \code{file_version = NULL}, which adds '.bkp' to the filename.
#' @return None
#' @export
#' @keywords helper

copy_file_version <- function(model_path, file_name, file_version = NULL) {
  if (!is.null(file_version)) {
    file_version <- paste0('.bkp', file_version)
  }
  file.copy(from = paste0(model_path, "/", file_name),
            to = paste0(model_path, '/', file_name, file_version),
            overwrite = TRUE)
}

#' Update Parameters
#'
#' This function updates parameters based on different types of changes.
#'
#' @param par Numeric vector: Original parameter values.
#' @param par_up Numeric vector: Values indicating the update for each parameter.
#' @param change Character scalar: Type of change to apply. Options are "relchg" for relative change,
#' "pctchg" for percentage change, "abschg" for absolute change, and "absval" for setting the absolute value change.
#'
#' @return Numeric vector with updated parameter values.
#'
#' @examples
#' par <- c(10, 20, 30, 40)
#' par_up <- c(0.1, 0.2, NA, 0.3)
#' update_par(par, par_up, "relchg") # Returns updated parameters with relative change applied.
#'
#' @export
#' @keywords helper

update_par <- function(par, par_up, change){
  par_up[!is.na(par_up)]
  if(change == "relchg") {
    par[!is.na(par_up)] <- par[!is.na(par_up)] * (1 + par_up[!is.na(par_up)])
  } else if(change == "pctchg") {
    par[!is.na(par_up)] <- par[!is.na(par_up)] * (1 + par_up[!is.na(par_up)]/100)
  } else if(change == "abschg") {
    par[!is.na(par_up)] <- par[!is.na(par_up)] + par_up[!is.na(par_up)]
  } else if(change == "absval") {
    par[!is.na(par_up)] <- par_up[!is.na(par_up)]
  }
  return(par)
}

#' Aggregate time series table
#'
#' `aggregate_time()` aggregates tables with a `date` column and multiple value
#' columns to coarser time steps, which are defined by `time` using the function
#' defined by `fun`.
#'
#' @param tbl A data frame that contains a 'date' column and multiple numerical
#'   columns.
#' @param time A function or a vector of functions to define the time intervals
#'   to which `tbl` is aggregated. Must be one or more of the `lubridate`
#'   functions `year`, `month`, `day`, or `yday`.
#' @param fun Aggregation function (e.g. `sum`, `mean`, `min`, or `max`).
#' @param average Should values be averaged after the aggregation? When e.g.
#'   the time interval is defined by `time = c(year, month)`, `average = TRUE`,
#'   will first aggregate to year and month and then average to mean monthy
#'   values.
#'
#' @importFrom data.table as.data.table
#' @importFrom dplyr bind_cols distinct left_join mutate select %>%
#' @importFrom lubridate day month yday year
#' @importFrom purrr map map_chr set_names
#' @importFrom tibble tibble
#' @importFrom tidyselect any_of
#'
#' @returns A tibble with the aggregated value columns and added time interval
#'   columns.
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(lubridate)
#'
#' # Generate dummy table
#'
#' date <- seq(ymd(19700101), ymd(20101231), by = 'day')
#'
#' n <- 100
#'
#' tbl <- map(1:n, ~ runif(length(date))) %>%
#' set_names(id_to_run(1:n)) %>%
#' bind_cols(.)
#'
#' tbl <- bind_cols(date = date, tbl)
#'
#' # Calculate maximum values for days of the year
#' aggregate_time(tbl, time = yday, fun = max)
#'
#' # Calculate mean monthly sums
#' aggregate_time(tbl, time = c(year, month), fun = sum, average = TRUE)
#'
#' # Calculate average annual sums
#' aggregate_time(tbl, time = year, fun = sum, average = TRUE)
#'
#' }
#'
#' @export
#'
aggregate_time <- function(tbl, time = year, fun = sum, average = FALSE) {
  date <- tbl$date
  date_comps <- map(c(time), ~.x(date))
  time_steps <- map_chr(date_comps, guess_time_step)

  if('yday' %in% time_steps & any(c('day', 'month') %in% time_steps)) {
    stop("When 'yday' is used you cannot use 'day' or 'month'.")
  }

  date_comps <- date_comps %>%
    set_names(time_steps) %>%
    bind_cols() %>%
    select(any_of(c('year', 'month', 'yday', 'day')))

  time_steps <- names(date_comps)

  if('month' %in% names(date_comps)) {
    date_comps$month <- month.abb[date_comps$month]
  }

  group_var <- apply(date_comps, 1, paste, collapse = '-')

  date_comps <- bind_cols(date_comps, grp = group_var) %>%
    distinct(., grp, .keep_all = TRUE)

  tbl <- tbl %>%
    select(., -date) %>%
    mutate(grp = group_var, .before = 1) %>%
    as.data.table(.) %>%
    .[,lapply(.SD,fun),by=grp] %>%
    tibble(.)

  tbl <- left_join(date_comps, tbl, by = 'grp') %>%
    select(-grp)

  if(average) {
    time_steps <- time_steps[-1]

    tbl <- tbl %>%
      select(-1) %>%
      as.data.table(.) %>%
      .[,lapply(.SD,mean),by=time_steps] %>%
      tibble(.)
  }

  return(tbl)
}

#' Guess the time step of a vector of values (e.g. years, days,...)
#'
#' @param x Numeric vector with year, month, or day values
#'
#' @returns A text string indicating the guessed time interval
#'
#' @keywords internal
#'
guess_time_step <- function(x) {
  d_x <- diff(x)
  min_d_x <- min(d_x)
  mean_int <- mean(diff(which(d_x != 0)))
  if(is.nan(mean_int)) mean_int <- length(x)
  if(mean_int > 31) {
    'year'
  } else if(mean_int > 1) {
    'month'
  } else if(min_d_x < -31) {
    'yday'
  } else {
    'day'
  }
}

#' Find Parameter Range for Simulation
#'
#' This function identifies the range of parameter values that produce simulation results
#' within a specified relative range of a target objective value. It also returns the parameter
#' value that minimizes the absolute error relative to the objective.
#'
#' @param par A numeric vector of parameter values.
#' @param sim A numeric vector of simulation results corresponding to the parameter values.
#' @param obj A numeric value representing the objective value to compare against.
#' @param rel_rng A numeric value specifying the relative range for acceptable error.
#' @importFrom stats approx
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{par_rng}{A numeric vector of length 2 containing the minimum and maximum parameter values
#'     within the specified relative range of the objective.}
#'     \item{par_val}{A numeric value of the parameter that minimizes the absolute error relative to the objective.}
#'     \item{x}{A numeric vector of interpolated parameter values.}
#'     \item{y}{A numeric vector of interpolated simulation results corresponding to \code{x}.}
#'   }
#'
#' @examples
#' \dontrun{
#' find_par_range(sim[[1]], calculate_wyr(sim), 0.27, 0.05)
#' }
#' @keywords internal

find_par_range <- function(par, sim, obj, rel_rng) {
  sim_interpol <- approx(par, sim, n = 500)
  sim_diff <- sim_interpol$y - obj
  sim_err  <- abs(sim_diff) / obj
  par_rng <- sim_interpol$x[which(sim_err < rel_rng)]
  par_val <- sim_interpol$x[which.min(sim_err)]

  if (length(par_rng) > 0) {
    par_rng <- c(min(par_rng), max(par_rng))
  } else {
    par_rng <- rep(sim_interpol$x[which.min(abs(sim_diff))], 2)
  }

  return(list(par_rng = par_rng,
              par_val = par_val,
              x = sim_interpol$x,
              y = sim_interpol$y))
}

#' Sum values of simulated variables
#'
#' `sum_variables()` sums the values of provided simulated variable tables.
#' The `date` and the `unit` columns are excluded. All other elements are summed
#' element wise. This is useful e.g. to calculate total Nitrogen from simulated
#' N fractions.
#'
#' @param ... Variable tables which should be summed up.
#' @importFrom dplyr bind_cols select
#' @importFrom purrr map
#' @return Returns a variable table with element wise summed values.
#'
#' @examples
#' \dontrun{
#' no3_sim  <- sim$simulation$no3_day
#' nh3_sim  <- sim$simulation$nh3_day
#' no2_sim  <- sim$simulation$no2_day
#' orgn_sim <- sim$simulation$orgn_day
#'
#' ntot_sim <- sum_variables(no3_sim, nh3_sim, no2_sim, orgn_sim)
#' }
#'
#' @export
#' @keywords helper
#'
sum_variables <- function(...) {
  date <- select(..1, any_of(c('unit', 'date')))
  var_list <- map(list(...), ~ select(.x, - any_of(c('unit', 'date'))))
  var_tbl <- Reduce('+', var_list)
  var_tbl <- bind_cols(date, var_tbl)
  return(var_tbl)
}

#' Convert run_names to integer ID values
#'
#' @param run_names Character vector providing the run names
#'
#' @returns An integer vector with the run IDs
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_sel <- paste0('run_', sprintf('%04d', c(1,36, 598, 2311)))
#' run_to_id(run_sel)
#' }
#'
run_to_id <- function(run_names) {
  as.integer(sub('run_', '', run_names))
}

#' Convert run ID values to run names
#'
#' @param run_ids Integer vector with the run IDs
#' @param id_max  Largest value for run IDs (defines the number of leading zeros)
#'
#' @returns An character vector with the run names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_sel <- c(1,36, 598, 2311)
#' id_to_run(run_sel)
#' }
#'
id_to_run <- function(run_ids, id_max = NULL) {
  if(is.null(id_max)) {
    n_char <- nchar(as.character(max(run_ids)))
    warning("Number of leading zeros guessed based on 'run_sel'")
  } else {
    n_char <- nchar(as.character(id_max))
  }
  paste0('run_', sprintf(paste0('%0', n_char, 'd'), run_ids))
}

#' Check and fix position of date column in a table
#'
#' @param tbl Data frame which for which date column is checked and fixed.
#'
#' @importFrom dplyr relocate
#' @importFrom lubridate is.Date
#' @importFrom purrr map_lgl
#'
#' @returns The `tbl` where the date column is put into first position and
#'   renamed to 'date'
#'
#' @keywords internal
#'
check_date_col <- function(tbl) {
  tbl_name <- as.character(substitute(tbl))

  date_col <- names(which(map_lgl(tbl, ~ is.Date(.x))))

  if(length(date_col) == 0) {
    stop("No date column found in '", tbl_name, "'.")
  }

  if(length(date_col) > 1) {
    stop("More than one date columns found in '", tbl_name, "'.")
  }

  names(tbl)[names(tbl) == date_col] <- 'date'
  tbl <- relocate(tbl, date, .before = 1)

  return(tbl)
}

#' Check a point value shape value if it is a correct value between 0 and 25
#'
#' @param shape Point shape value. To be a correct value it must be one of 0:25.
#' Any other value returns NA
#'
#' @returns The correct point shape value or NA.
#'
#' @keywords internal
#'
check_point_shape <- function(shape) {
  if(is.null(shape)) {
    NA
  } else if(shape %in% 0:25) {
    shape
  } else {
    NA
  }
}

#' Add a time stamp to the save name
#'
#' @param save_name Name of the saved simulation run
#'
#' @returns The name of the simulation run with an added time stamp
#'
#' @export
#'
add_timestamp <- function(save_name) {
  paste0(format(Sys.time(), '%Y%m%d%H%M'),'_', save_name)
}

#' Replace hvkl operations with harv + kill
#'
#' @param project_path Character string, path to the SWAT+ project folder
#'   (i.e. TxtInOut).
#' @return updated management.sch file
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_replace str_count
#' @export
#'
#' @examples
#' \dontrun{
#' add_kill_op('path_to_your_model')
#' }

add_kill_op <- function(project_path){
  mgt_sch <- read_lines(paste0(project_path,'/management.sch'), lazy = FALSE)
  l <- 0
  while(length(grep("hvkl", mgt_sch)) != 0){
    i <- grep("hvkl", mgt_sch)[1]
    if(as.numeric(strsplit(mgt_sch[i], " +")[[1]][3])>0 | as.numeric(strsplit(mgt_sch[i], " +")[[1]][4])>0 ){
      kill_line <- str_replace(mgt_sch[i], "hvkl", "kill") %>%
        str_replace(c("forest_cut|grain1|grain|grass_bag|grass_mulch|hay_cut_high|hay_cut_low|orchard|peanuts|
              silage|stover_high|stover_los|stover_med|tuber|vegetables"), "null")
      mgt_sch[i] <- str_replace(mgt_sch[i], "hvkl", "harv")
      mgt_sch <- insert_line_at(mgt_sch, kill_line, insert_after=i)
      l <- l+1
    } else {
      ##Changing hvkl to harv and kill operations
      kill_line <- str_replace(mgt_sch[i], "hvkl", "kill") %>%
        str_replace(c("forest_cut|grain1|grain|grass_bag|grass_mulch|hay_cut_high|hay_cut_low|orchard|peanuts|
              silage|stover_high|stover_los|stover_med|tuber|vegetables"), "null") %>%
        str_replace_all("[:digit:]", "0") %>%
        str_replace("0.00000", "0.00001") ## Kill operation at 0.00001 HU, next day after harvest.
      mgt_sch[i] <- str_replace(mgt_sch[i], "hvkl", "harv")
      mgt_sch <- insert_line_at(mgt_sch, kill_line, insert_after=i)
      l <- l+1
    }
    ## Adding increased counter
    ii <- i - 1
    ##Fixing counter
    while (str_count(mgt_sch[ii], "\\S+") != 3 & ii != 0) {
      ii <- ii - 1
    }
    ##Adding one additional operation
    c <- strsplit(mgt_sch[ii], " +")[[1]]
    mgt_sch[ii] <-  paste0(c[1], "                           ", as.numeric(c[2])+1, "          ", c[3], "  ")
  }
  if(l > 0){
    file.copy(paste0(project_path,'/management.sch'), paste0(project_path,'/management_bak.sch'))
    write_lines(mgt_sch, paste0(project_path,'/management.sch'))
    return(paste0(l, " lines were updated (`hvkl` changed to `harv`) and same number added (with `kill` operations) in 'management.sch'. Original file is backed up in 'management_bak.sch'."))
  } else {
    return("No `hvkl` operations exist in 'management.sch'. File was not changed.")
  }
}

#' Insert line into character with multiple lines
#'
#' @param dat character with multiple lines
#' @param txt character line to be inserted
#' @param insert_after numeric for line number after, which to insert txt
#'
#' @return character with multiple lines with inserted new lines
#' @keywords internal

insert_line_at <- function(dat, txt, insert_after){
  pre <- dat[1:insert_after]
  if(length(dat) > insert_after){
    post <- dat[(insert_after+1):length(dat)]
    return(c(pre, txt, post))
  } else {
    return(c(pre, txt))
  }
}
