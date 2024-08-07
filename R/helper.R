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
#' @keywords internal

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


#' Generate ID Text Strings for Parameter Names
#'
#' This function generates ID text strings to be included in parameter names based on
#' parameter groups and hydrology data.
#'
#' @param par A character vector specifying the parameter name. Example 'perco'.
#' @param par_groups A vector of groups identified based on parameter value.
#' Example: init_perco <- c(low = 0.01, mod = 0.50, high = 0.95)
#' @param hyd A data frame containing 'hydrology.hyd' data.
#' @importFrom purrr map_vec
#'
#' @return A vector of HRU ID text strings separated into groups according to
#' provided values.
#' @export
#' @examples
#' \dontrun{
#' hyd_hyd <- read_tbl(paste0(model_path, '/hydrology.hyd'))
#' init_perco <- c(low = 0.01, mod = 0.50, high = 0.95)
#' id_text_strings('perco', init_perco, hyd_hyd)
#' }

id_text_strings <- function(par, par_groups, hyd){
  map_vec(par_groups, ~group_values(which(hyd[[par]] == .x)))
}

# Performance calculation ------------------------------------------------------

#' Fix the dates
#'
#' Adjust the periods of the observation and simulation data so that they match.
#'
#' @param runr_obj SWATrunR object
#' @param obs_obj Observation dataframe with two columns named "date" and "value"
#' @param trim_start (optional) The starting date to trim the data times series.
#'  Default \code{trim_start = NULL}. Example: trim_start = "2010-01-01"
#' @param trim_end (optional) The ending date to trim the data times series.
#' Default \code{trim_end = NULL}. Example trim_end = "2015-01-01"
#' @importFrom dplyr filter %>%
#' @importFrom tidyr drop_na
#'
#' @return list with two elements: "simulation" SWATrunR object and
#' "observation" dataframe with the same dates period.
#' @export
#'
#' @examples
#' \dontrun{
#' my_obj <- fix_dates(runr_obj = sim_flow, obs_obj = obs_wq,
#' trim_start = "2010-01-01", trim_end = "2015-01-01")
#' }
#' @keywords data manipulation

fix_dates <- function(runr_obj, obs_obj, trim_start = NULL, trim_end = NULL){
  # Just to be sure observations has the same date format as simulation
  obs_obj$date <- as.Date(obs_obj$date)

  if(sum(is.na(obs_obj$value)) | sum(is.na(obs_obj$date)) > 0){
    warning(paste0("There are", sum(is.na(obs_obj$value)) + sum(is.na(obs_obj$date)),
                   "missing values in the observation data. Lines with missing
                   values will be removed."))
    obs_obj <- obs_obj %>% drop_na()
  }
  # Name for the first simulation parameter saved
  n <- names(runr_obj$simulation)
  # Get the dates of the simulation
  all_sim_dates <- runr_obj[["simulation"]][[n[1]]][["date"]]
  cat(paste0("Simulation period ",min(all_sim_dates), " - ", max(all_sim_dates),
             ", \n observation period is ", min(obs_obj$date), " - ",
             max(obs_obj$date), ".\n"))
  # Filter the observation data to the same time period as the simulation data
  if(min(all_sim_dates)>max(obs_obj$date) | max(all_sim_dates)<min(obs_obj$date)){
    stop("Simulation and observed data period do not overlap.")
  }
  obs_obj <- obs_obj %>%
    filter(date >= min(all_sim_dates) &
             date <= max(all_sim_dates) &
             date %in% all_sim_dates)
  for (n1 in n){
    # Filter the simulation data to the same time period as the observation data
    runr_obj[["simulation"]][[n1]] <- runr_obj[["simulation"]][[n1]] %>%
      filter(date %in% obs_obj$date)
  }

  # Providing the output period
  all_sim_dates <- runr_obj[["simulation"]][[n[1]]][["date"]]

  # Trim simulation and observation data to given dates
  if(!is.null(trim_start)|!is.null(trim_end)){
    # making sure of format
    trim_start <- as.Date(trim_start)
    trim_end <- as.Date(trim_end)

    #
    if(min(all_sim_dates)>trim_start | is.null(trim_start)){
      trim_start <- min(all_sim_dates)
    }
    if(max(all_sim_dates)<trim_end | is.null(trim_end)){
      trim_end <- max(all_sim_dates)
    }
    # Filter the simulation data to the same time period as the observation data
    for (n1 in n){
      runr_obj[["simulation"]][[n1]] <- runr_obj[["simulation"]][[n1]] %>%
        filter(date >= trim_start,
               date <= trim_end)
    }
    # Providing the output period
    all_sim_dates <- runr_obj[["simulation"]][[n[1]]][["date"]]

    obs_obj <- obs_obj %>%
      filter(date %in% all_sim_dates)

  }

  print(paste0("Simulation and observation period is filtered to ",
               min(all_sim_dates), " - ", max(all_sim_dates), "."))
  # Check if the number of rows in the observation and simulation data match
  if(nrow(obs_obj) != nrow(runr_obj[["simulation"]][[n[1]]])){
    stop("Function fix_dates() failed. The number of rows in the observation
  and simulation data do not match. This might be due to the fact that the
       multible observations for one day are present in the observation data.
         Please check the observation data and correct this.")
  }
  return(list(sim=runr_obj, obs=obs_obj))
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
#' @importFrom tibble tibble
#' @importFrom dplyr mutate %>%
#'
#' @examples
#' \dontrun{
#' fdc <- calc_fdc(c(3, 1, 4, 1, 5, 9, 2, 6, 5))
#' }
#' @keywords internal

calc_fdc <- function(x) {
  if(is.vector(x)) {
    x <- tibble(value = x)
  }

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
#' @keywords internal


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

#' Aggregate and Average Annually
#'
#' This function takes a data frame containing a date column and other numerical
#' columns, aggregates the data annually, and then computes the average of the
#' annual sums.
#'
#' @param tbl A data frame that contains a 'date' column and other numerical columns.
#' @importFrom dplyr mutate select group_by summarise everything across
#' @importFrom lubridate year
#' @return A numeric vector with the mean of the annual sums for each column in the input data frame.
#'
#' @examples
#' \dontrun{
#' flo <- aggregate_aa(sim$simulation$flo)
#' }
#' @keywords internal

aggregate_aa <- function(tbl) {
  tbl %>%
    mutate(year = year(date)) %>%
    select(-date) %>%
    group_by(year) %>%
    summarise(across(.cols = everything(), .fns = sum)) %>%
    summarise(across(.cols = everything(), .fns = mean)) %>%
    select(-year) %>%
    unlist(.)
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


