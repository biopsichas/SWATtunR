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
#'
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
#' @keywords calculate

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

#' @keywords calculate

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
#' @keywords calculate

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
#' @examples
#' \dontrun{
#' hyd_hyd <- read_tbl(paste0(model_path, '/hydrology.hyd'))
#' init_perco <- c(low = 0.01, mod = 0.50, high = 0.95)
#' id_text_strings('perco', init_perco, hyd_hyd)
#' }

id_text_strings <- function(par, par_groups, hyd){
  map_vec(par_groups, ~group_values(which(hyd[[par]] == .x)))
}
