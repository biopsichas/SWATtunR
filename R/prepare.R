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

#' Sample parameters with Latin Hypercube Sampling (LHS)
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

#' Sample parameter values one-at-a-time (OAT)
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

#' Sample days to maturity values
#'
#' `sample_days_mat()` reads the initial days to maturity values for the
#' selected `crop_names` from the unchanged plants.plt file (saved in
#' ./backup/plants.plt) and generates a parameter table which can be used with
#' the `SWATrunR` function `run_swatplus()` perform simulations with changed
#' days to maturity values for the selected crops.
#'
#' @param crop_names Character vector with names of selected crops. All crops
#'   must be defined in the plants.plt backup file saved in ./backup/plants.plt
#' @param change_min Minimum value of change for the days to maturity. Default
#'   is -60.
#' @param change_max Minimum value of change for the days to maturity. Default
#'   is 90.
#' @param change_step Minimum value of change for the days to maturity. Default
#'   is 10.
#'
#' @returns A parameter input table for the input argument `parameter` of the
#'   `SWATrunR` function `run_swatplus()`.
#'
#' @export
#'
#' @importFrom dplyr filter mutate select %>%
#' @importFrom purrr map_df set_names
#' @importFrom tibble deframe
#'
#' @examples
#' \dontrun{
#' library(SWATtunR)
#'
#' # Crops for which days to maturity values are sampled
#' crop_names <- c('csil', 'wbar', 'sgbt', 'wwht')
#'
#' # Generate a days to maturity parameter table
#' par_dmat <- sample_days_mat(crop_names)
#'
#' par_dmat
#' }
sample_days_mat <- function(crop_names, change_min = -60, change_max = 90,
                        change_step = 10) {
  # Reading the plants.plt input file from the initial unchanged backup file
  plants_plt <- read_tbl(file_path = './backup/plants.plt')

  # Check if all crop_names are defined in plants.plt
  crop_missing <- !crop_names %in% plants_plt$name

  if(any(crop_missing)) {
    stop("The following 'crop_name's are not defined in 'plants.plt':\n",
         paste(crop_names[crop_missing], collapse = ', '))
  }

  # Get a vector with days_mat initial values for the selected crops
  dmat_init <- plants_plt %>%
    filter(name %in% crop_names) %>%
    select(name, days_mat) %>%
    ## if days_mat is 0, set it to 110 (the default value in the SWAT+ code)
    mutate(days_mat = ifelse(days_mat == 0, 110, days_mat)) %>%
    deframe() %>%
    .[match(names(.), crop_names)]

  dmat_chg <- round(seq(change_min, change_max, change_step))

  # Generate the parameter input table with the changes for all crops and
  # convert the names into SWATrunR syntax
  par_dmat <- map_df(dmat_init, ~ .x + dmat_chg) %>%
    set_names(., paste0('days_mat_', names(.),
                        '::days_mat.pdb | change = absval | name = ' ,
                        names(.)))

  return(par_dmat)
}

#' Rearrange parameter change tables to SWATrunR format
#'
#' @param par_tbl A table with parameter changes where the first column is
#' 'plant_name' and the following columns are the parameters change definitions.
#'
#' @returns The resturctured SWATrunR parameter table
#'
#' @importFrom dplyr  %>%
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_wider
#' @importFrom purrr set_names
#' @importFrom tidyselect matches
#'
#' @export
#'
prepare_plant_parameter <- function(par_tbl) {
  crop_names <- par_tbl$plant_name
  par_names  <- str_remove(names(par_tbl)[-1], '\\..*')
  par_names  <- paste0(rep(par_names, each = length(crop_names)), '_',
                       rep(crop_names, length(par_names)))
  par_tbl    <- pivot_wider(par_tbl,
                            names_from = plant_name,
                            values_from = matches('.pdb'),
                            names_glue = "{.value} | name = {plant_name}") %>%
    set_names(paste0(par_names, '::', names(.)))

  return(par_tbl)
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
#' @importFrom purrr map_int
#' @export
#'
group_hydr_values <- function(par_name, model_path) {
  hyd_hyd <- read_tbl(paste0(model_path, '/hydrology.hyd'))
  # if there is an empty line at the end of the hydrology.hyd file, this
  # function fails. Adding this filter makes sure that only rows with real data
  # are loaded, as these should all be prefixed with "hyd" in the name column.
  hyd_hyd %>% filter(grepl(x = name, pattern = "hyd")) -> hyd_hyd
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

  if(length(par_bound) != length(unique(par_group))) {
    stop(paste0("Error in translation of parameter values for", par_name, ":\n",
                "'par_bound' defines bundaries for ", length(par_bound), " parameter values groups.\n",
                "'par_group' has ", length(unique(par_group)), " individual parameter value groups.\n",
                "Please provide the same number of elements for both input arguments."))
  }

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
#' @param funs A list of functions used to calculate goodness-of-fit (GoF) values.
#' This can be a named or unnamed list. If named (e.g., `list(nse_q = NSE, pb_q = pbias)`),
#' the names will be used as column names in the returned results. If unnamed,
#' the function names themselves will be used as column names. Commonly used
#' functions (such as `NSE`, `pbias`, `KGE`, etc.) are available in the
#' documentation of the *hydroGOF* package.
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
#' @seealso For available goodness-of-fit functions, see the [hydroGOF package documentation](https://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf).

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
