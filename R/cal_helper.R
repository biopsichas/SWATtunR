# Write 'calibration.cal' helpers ------------------------------------------------------
# Functions taken from SWATrunR package github.com/chrisschuerz/SWATrunR

#' Translate the parameter inputs into a parameter input table and a separarate
#' table providing the file constraints and the filter expressions for the
#' respective parameter
#'
#' @param parameter Model parameters as named vector or tibble
#' @importFrom purrr map_dfc
#'
#' @keywords internal
#'
format_swatplus_parameter <- function(parameter) {
  if(!any(names(parameter) %in% c("values", "definition"))) {
    par_constrain <- translate_parameter_constraints(names(parameter), 'plus')
    names(parameter) <- par_constrain$par_name
    if(!is.data.frame(parameter)) parameter <- map_dfc(parameter, ~.x)
    return(list(values = parameter, definition = par_constrain))
  } else {
    return(parameter)
  }
}


#' Update the calibration file structure with the parameter set of the current
#' simulation run_i
#'
#' @param thread_path Path to the current parallel thread 'thread_i'
#' @param parameter Model parameters as named vector or tibble
#' @param calibration Template table structure of the calibration file
#' @param i_run Index of the i_th simulation run
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map2_df map_dbl
#' @importFrom readr write_lines
#' @importFrom stringr str_sub str_trim
#'
#' @keywords internal
#'
write_calibration <- function(thread_path, parameter, calibration, run_index,
                              i_run) {
  is_plant_par <- parameter$definition$file_name == 'pdb'

  if(any(is_plant_par)) {
    update_plant_par(thread_path, parameter, is_plant_par, run_index, i_run)
  }
  # Remove all pdb (plant) parameters from the parameter list and keep all
  # parameters which are updated with the calibration.cal file
  parameter$definition <- parameter$definition[!is_plant_par,]
  parameter$values     <- parameter$values[ ,!is_plant_par]
  if(nrow(parameter$definition) > 0) {
    cal_pos <- which(is.na(calibration$VAL))
    # cal_names <- calibration$NAME[cal_pos]

    calibration$VAL[cal_pos] <- parameter$values[run_index[i_run],] %>%
      unlist(.) %>%
      # map_dbl(., ~.x) %>%
      set_names(., parameter$definition$parameter) %>%
      sprintf("%.15f", .) %>%
      str_sub(., 1, 15)

    col_format <- c("%-12s", "%8s", "%16s", "%16s", rep("%8s", ncol(calibration) - 4))

    col_names <- names(calibration) %>%
      sprintf(col_format, .) %>%
      paste(., collapse = "") %>%
      str_remove_all(., 'OBJ\\_[:digit:]') %>%
      str_trim(.)

    calibration <- map2(calibration, col_format, ~sprintf(.y, .x)) %>%
      map_df(., ~ str_replace_all(.x, 'NA', '')) %>%
      apply(., 1, paste, collapse = "") %>%
      c("Number of parameters:", sprintf("%2d",length(cal_pos)), col_names, .) %>%
      str_trim(.)

    write_lines(calibration, thread_path%//%"calibration.cal")
  } else {
    if(file.exists(thread_path%//%"calibration.cal")) {
      file.remove(thread_path%//%"calibration.cal")
    }
  }
}

#' Translate the parameter inputs and the set constraints into lookup table
#'
#' @param par Character string vector providing parameters and constraints
#'
#' @importFrom dplyr %>% bind_cols last_col relocate select
#' @importFrom purrr map map_chr map_df map_if map2_lgl map2 map2_chr
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_remove str_remove_all str_split
#' @keywords internal
#'
translate_parameter_constraints <- function(par, swat_vers) {

  has_par_name <- str_detect(par, "\\:\\:")
  par_list <- str_split(par, '\\:\\:|\\|')

  has_change   <- map2_lgl(par_list, has_par_name, ~ str_detect(.x[2+.y], "change"))
  if(any(!has_change)) {
    stop("Type of change definition is incorrect for the following parameters:\n  ",
         map_chr(par_list[!has_change], ~.x[1]) %>% paste(., collapse = ", "), "\n  ",
         "The parameter change must be provided for all parameters at the second\n  ",
         "position of the parameter name (after the first '|', e.g. 'par | change = abschg').")
  }

  par_name <- map(par_list, ~.x[1]) %>%
    map_if(., !has_par_name, ~ str_remove(.x, '\\..*')) %>%
    unlist() %>%
    trimws(.)
  parameter <- map2_chr(par_list, has_par_name, ~.x[1 + .y]) %>%
    map_chr(., ~ str_remove(.x, '\\..*')) %>%
    trimws(.)
  file_name <- map2_chr(par_list, has_par_name, ~.x[1 + .y]) %>%
    map_chr(., ~ str_remove(.x, '.*\\.')) %>%
    trimws(.)
  change <- par_list %>%
    map2_chr(., has_par_name, ~ str_remove_all(.x[2+.y], 'change|\\=')) %>%
    trimws(.)
  model_par <- tibble(par_name, parameter, file_name, change, full_name = par)

  if(any(!(change %in% c("relchg", "pctchg", "abschg", "absval")))) {
    stop("Wrong input for change type. Must be either"%&&%
           "'relchg', 'pctchg', 'abschg', or 'absval'.")
  }

  if(swat_vers == '2012') {
    swat_files <- c("pnd", "rte", "sub", "swq", "hru", "gw", "mgt", "sol", "chm",
                    "sdr", "sep", "bsn", "wwq", "res", "ops")
  } else {
    swat_files <- c("hru", "sol", "bsn", "swq", "rte", "res", "aqu", "hlt", "pst",
                    "plt", "pdb")
  }

  if(any(!(file_name %in% swat_files))) {
    stop(paste(file_name[!(file_name %in% swat_files)], collapse = ", ")%&&%
           "files are no valid file type!")
  }
  unique_par <- table(par_name)
  if(any(unique_par > 1)) {
    stop("Duplicated parameter names found! Define individual names with:\n"%&%
           "par_name::parameter|...")
  }

  bool_op <- c("\\=\\=", "\\!\\=", "\\<\\=", "\\>\\=", "\\=", "\\<", "\\>", "\\%in\\%")

  cons_list <- map2(par_list, has_par_name, ~.x[-(1:(2+.y))])

  constraints <- cons_list %>%
    map_df(., ~ build_constraint_tbl(.x, bool_op)) %>%
    remove_dummy() %>%
    map_df(., ~map_chr(.x, ~ tidy_constraint(.x, bool_op)))

  if(ncol(constraints) > 0) {
    if(swat_vers == '2012') {
      cons_var <- c("sub", "hru", "luse", "soil", "slope", 'layer')
    } else {
      # stop('Parameter constraints not yet implemented!')
      cons_var <- c('unit', 'lyr', 'year', 'day', 'hsg', 'plant', 'texture',
                    'landuse', 'slope', # according to Nancy also 'region' >> check at later step, check also conditions for year and day with Nancy
                    'name', 'plnt_typ', 'gro_trig') # Added 3 filter variables for the plants data base parameters
      # cons_var <- c("hru", "sol", "bsn", "swq", "rte", "res", "aqu", "hlt", "pst")
    }
    if(any(!(names(constraints) %in% cons_var))) {
      stop('The parameter constraints: ', paste(names(constraints)[!(names(constraints)
                                                                     %in% cons_var)], collapse = ", "),
           " are not valid!")
    }
    model_par <- bind_cols(model_par, constraints) %>%
      relocate(., full_name, .after = last_col())
  }
  # is_constraint_var <- names(constraints) %in% c("sub", "hru", "luse", "soil", "slope")

  # if (!) {
  #   stop("The")
  # }


  return(model_par)
}

#' Read the unit numbers (for hru, aqu, cha, res) and the textures etc for later
#' parameter conditioning.
#'
#' @param project_path Path to the SWAT+ project
#'
#' @importFrom dplyr filter select %>%
#' @importFrom purrr map
#' @importFrom readr read_lines
#'
#' @keywords internal
#'
read_unit_conditions <- function(project_path, parameter) {
  if('unit' %in% names(parameter$definition)) {
    unit_cond <- parameter$definition %>%
      select(file_name, unit) %>%
      filter(!is.na(unit)) %>%
      .$file_name %>%
      unique(.)
  } else {
    unit_cond <- NULL
  }

  units <- list()
  if ('hru' %in% unit_cond) {
    units$hru <- get_tbl_column(project_path%//%'hru-data.hru', 'id')
  }
  if ('sol' %in% unit_cond) {
    units$sol <- get_tbl_column(project_path%//%'hru-data.hru', 'id')
  }
  if ('cha' %in% unit_cond) {
    cha_file <- list.files(project_path, pattern = 'channel.*\\.cha')[1] # maybe removed when clear which the final channel file is.
    units$cha <- get_tbl_column(project_path%//%cha_file, 'id')
  }
  if ('res' %in% unit_cond) {
    units$res <- get_tbl_column(project_path%//%'reservoir.res', 'id')
  }
  if ('aqu' %in% unit_cond) {
    units$aqu <- get_tbl_column(project_path%//%'aquifer.aqu', 'id')
  }
  #swq Not yet considered,
  # Remaining two object types hlt and pst also not yet implemented.
  conds <- list(hsg = LETTERS[1:4],
                texture = get_sol_texture(project_path%//%'soils.sol'),
                plant   = get_tbl_column(project_path%//%'plants.plt', 'name') %>% unique(),
                landuse = get_tbl_column(project_path%//%'landuse.lum', 'plnt_com') %>% unique()
  )
  return(list(units = units, conds = conds))
}


#' Acquire the object indices of from the respective object file.
#'
#' @param file Path to the object file
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_table2 cols col_character col_double
#'
#' @keywords internal
#'
get_tbl_column <- function(file, col_i) {
  suppressWarnings(read_table2(file, skip = 1,
                               col_types = cols(id = col_double(),
                                                .default = col_character()))) %>%
    .[[col_i]]
}

#' Setup the calibration.cal file and include all parameter conditions
#' of the simulation and set them according to respective input parameters
#'
#' @param par_def Tibble with one line that includes the parameter definition of
#'   parameter i
#' @param unit_cons List of tibbles that contains the meta information of hru, aqu,
#'   etc. units and constrain variables (e.g. texture, plant)
#'
#' @importFrom dplyr bind_rows select %>%
#' @importFrom purrr map map2_df
#' @importFrom tibble tibble
#' @importFrom tidyselect any_of vars_select_helpers
#'
#' @keywords internal
#'
setup_calibration_cal <- function(par_def, unit_cons) {
  par_cal <- init_cal(par_def)

  if('lyr' %in% names(par_def)) {
    if(!is.na(par_def$lyr)) {
      par_cal <- add_value_range(par_cal, par_def$lyr, 'LYR')
    }
  }
  if('year' %in% names(par_def)) {
    if(!is.na(par_def$year)) {
      par_cal <- add_value_range(par_cal, par_def$year, 'YEAR')
    }
  }
  if('day' %in% names(par_def)) {
    if(!is.na(par_def$day)) {
      par_cal <- add_value_range(par_cal, par_def$day, 'DAY')
    }
  }
  if('unit' %in% names(par_def)) {
    if(!is.na(par_def$unit)) {
      par_cal <- add_obj(par_cal, par_def$unit,
                         unit_cons$units[[par_def$file_name]])
    }
  }

  if(any(c('hsg', 'texture', 'plant', 'landuse') %in% names(par_def))) {
    soil_luse <- select(par_def, any_of(c('hsg', 'texture', 'plant', 'landuse'))) %>%
      select(., !vars_select_helpers$where(is.na)) %>% # will be replaced when where is in the tidyselect namespace
      map(., ~ .x)
    cond_tbl <- map2_df(soil_luse, names(soil_luse), ~ add_soil_luse(.x, .y, unit_cons$conds))
  } else {
    cond_tbl <- tibble()
  }

  if(any('slope' %in% names(par_def))) {
    if(!is.na(par_def$slope)) {

      cond_tbl <- add_slope(par_def$slope, cond_tbl)
    }
  }

  if(any(c('hsg', 'texture', 'plant', 'landuse', 'slope') %in% names(par_def))) {
    par_cal$CONDS <- as.character(nrow(cond_tbl))
    par_cal <- bind_rows(par_cal, cond_tbl)
  }

  return(par_cal)
}

#' Add the unit values for which objects the parameter change should be applied
#'
#' @param par_cal The calibration.cal tibble for the parameter i
#' @param unit Character string that defines condition for the object units (ids of e.g. hru objects)
#' @param unit_all List of vectors that define the unit ids of all object types.
#'
#' @importFrom dplyr bind_cols bind_rows %>%
#' @importFrom purrr set_names
#'
#' @keywords internal
#'
add_obj <- function(par_cal, unit, unit_all) {
  eval_unit <- paste('unit_all', unit) %>%
    parse(text = .) %>%
    eval(.)

  unit <- unit_all[eval_unit] %>%
    unique(.) %>%
    sort(.) %>%
    identify_sequence(.) %>%
    set_names(., 'OBJ'%_%1:length(.)) %>%
    bind_rows()

  par_cal$OBJ_TOT <- length(unit)
  par_cal <- bind_cols(par_cal, unit)

  return(par_cal)
}

#' Add condition lines based on slope
#'
#' @param cond Character string that defines condition for the variable 'var'
#' @param cond_tbl Tibble with conditions defined based on soil and land use
#'
#' @importFrom dplyr bind_rows %>%
#' @importFrom stringr str_extract str_remove str_sub str_trim
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
add_slope <- function(cond, cond_tbl) {
  cond_op  <- str_extract(cond, '<\\=|\\>\\=|\\=\\=|\\=|\\<|\\>')
  cond_val <- str_remove(cond, cond_op) %>% str_trim(.) %>% as.numeric()
  if(is.na(cond_val)) {
    stop("Parameter condition 'slope ", cond, "' is not allowed.")
  }

  slp_tbl <- tibble(NAME = 'slope',
                    CHG_TYPE = str_sub(cond_op, 1, 1),
                    VAL = cond_val)
  cond_tbl <- bind_rows(cond_tbl, slp_tbl)

  return(cond_tbl)
}

#' Add the value range 'val' for the condition variable 'var' for a parameter to
#' calibration.cal
#'
#' @param par_cal The calibration.cal tibble for the parameter i
#' @param val Vector that defines the value range
#' @param var Character string. Name of the variable
#'
#' @keywords internal
#'
add_value_range <- function(par_cal, val, var) {
  val_range <- get_value_range(val)
  par_cal[var%&%'1'] <- val_range[1]
  par_cal[var%&%'2'] <- val_range[2]
  return(par_cal)
}

#' Acquire the object indices of from the respective object file.
#'
#' @param file Path to the object file
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_chr
#' @importFrom readr read_lines
#' @importFrom stringr str_split str_subset str_trim
#'
#' @keywords internal
#'
get_sol_texture <- function(file) {
  read_lines(file, lazy = FALSE) %>%
    .[-c(1,2)] %>%
    str_subset(.,'^[:graph:]') %>%
    str_trim(.) %>%
    str_split(., '[:space:]+') %>%
    map_chr(., ~.x[7])
}

#' Initialize the calibration.cal table
#'
#' @param par_def Tibble with one line that includes the parameter definition of
#'   parameter i
#' @importFrom dplyr mutate select %>%
#' @importFrom purrr set_names
#' @keywords internal
#'
init_cal <- function(par_def) {
  par_def %>%
    select(., parameter, change) %>%
    set_names(c("NAME", "CHG_TYPE")) %>%
    mutate(VAL = NA_real_, CONDS = '0', LYR1 = 0, LYR2 = 0, YEAR1 = 0, YEAR2 = 0,
           DAY1 = 0, DAY2 = 0, OBJ_TOT = 0)
}

#' Remove the generated dummy column that was required for the col bind
#'
#' @param tbl Tibble
#' @importFrom dplyr select
#' @keywords internal
#'
remove_dummy <- function(tbl) {
  if('dummy' %in% names(tbl)) {
    select(tbl, -dummy)
  } else {
    tbl
  }
}

#' Modify plants.plt parameters
#'
#' @param thread_path Path to the parallel thread folder
#' @param parameter List providing the parameter table and the parameter
#'   constraints
#' @param is_plant_par Logical vector that defines the plant parameters
#' @param run_index Vector of the indices of runs that are performed
#' @param i_run Index that gives the number of the current run simulated in the
#'   respective thread
#'
#' @importFrom data.table fwrite
#' @importFrom dplyr %>% filter mutate select
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
update_plant_par <- function(thread_path, parameter, is_plant_par, run_index, i_run) {
  def <- parameter$definition[is_plant_par, ]
  plant_par <- parameter$plants_plt %>%
    mutate(., file_name = 'pdb', file_code = 1:nrow(.))
  for (i_par in 1:nrow(def)) {
    def_i <- def[i_par, ]
    idx <- def_i %>%
      build_expression() %>%
      evaluate_expression(plant_par, .) %>%
      .[["file_code"]]

    par_up_i <- parameter$values[[def_i$par_name]][run_index[i_run]]
    par_val  <- plant_par[[def_i$parameter]][idx]

    par_val <- update_par(par_val, par_up_i, def_i$change)

    plant_par[[def_i$parameter]][idx] <- par_val

  }
  plant_par <- select(plant_par, - file_name, - file_code)
  plt_path <- paste0(thread_path, '/plants.plt')
  write_lines('plants.plt updated with SWATrunR', file = plt_path)
  fwrite(plant_par, plt_path, append = TRUE, sep = '\t', col.names = TRUE)
}

#' Group sequences of units together for writing the OBJ columns in calibration.cal
#'
#' @param val Numeric vector with the unit values
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 reduce
#'
#' @keywords internal
#'
identify_sequence <- function(val) {
  split_end <- which(diff(val) > 1)
  split_start <- c(1, split_end + 1)
  split_end <- c(split_end, length(val))
  map2(split_start, split_end, ~ val[.x:.y]) %>%
    map(., ~ translate_sequence(.x)) %>%
    reduce(., c)
}

#' Get the value range from a condition in the par_def table
#'
#' @param condition Character string that defines a condition
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_detect str_remove
#' @importFrom purrr set_names
#' @keywords internal
#'
get_value_range <- function(condition) {
  if(str_detect(condition, '%in%')) {
    condition <- condition %>%
      str_remove(., '%in%') %>%
      parse(text = .) %>%
      eval(.)
  } else if (str_detect(condition, '==')) {
    condition <- condition %>%
      str_remove(., '==') %>%
      as.numeric(.)
  } else {
    stop("For parameter conditioning with 'lyr', 'year', and 'day' only single",
         "values or upper lower bound implemented yet!")
  }
  return(c(min(condition), max(condition)))
}

#' Build filter expressions from the parameter constraint table'
#'
#' @param constraints Constraint table
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom purrr map map_chr map2_chr transpose
#' @importFrom tidyselect any_of
#' @keywords internal
#'
build_expression <- function(constraints) {
  constraints %>%
    mutate(file_name = paste0("== '", file_name, "'")) %>%
    select(-par_name, -parameter, - change, - full_name, - any_of('layer')) %>%
    transpose() %>%
    map(., ~.x[!is.na(.)]) %>%
    map(., ~map2_chr(.x, names(.), ~ paste0('filter(., ',.y, .x, ')'))) %>%
    map(., ~ c("table", .x)) %>%
    map_chr(., ~ paste(.x, collapse = " %>% "))
}

#' Evaluate the expression defined for a variable in 'output'
#'
#' @param table Table to which dplyr expression should be applied
#' @param expr Expression to be applied to table
#'
#' @importFrom dplyr %>%
#' @keywords internal
#'
evaluate_expression <- function(table, expr){
  parse(text = expr) %>%
    eval(.)
}

#' Build the parameter constraint from the rules in the parameter names
#'
#' @param cons_i Text string that defines the constraint i
#' @param bool_op Vector of strings that define the different possible boolean operations
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_df set_names
#' @importFrom stringr str_extract str_remove
#' @keywords internal
#'
build_constraint_tbl <- function(cons_i, bool_op){
  if(all(is.na(cons_i))) {
    tibble(dummy = NA)
  } else {
    rex <- paste(bool_op%.%"*", collapse = "|")
    cons_names <- str_remove(cons_i, rex) %>%
      trimws()

    str_extract(cons_i, rex) %>%
      set_names(., cons_names) %>%
      map_df(., ~.x)
  }
}

#' Apply a set of operations to the rule strings to check and clean the defined operations
#'
#' @param chr Text string that defines the constraint i
#' @param bool_op Vector of strings that define the different possible boolean operations
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map_chr map_lgl map_if
#' @importFrom stringr str_extract str_remove str_remove_all str_split
#' @keywords internal
#'
tidy_constraint <- function(chr, bool_op) {
  rex <- paste(bool_op, collapse = "|")
  op <- str_extract(chr, rex) %>%
    ifelse(. == '=', '==', .)
  chr_sep <- str_remove(chr, rex)
  num_eval <- try(eval(parse(text = chr_sep)), silent = T)
  is_num <- is.numeric(num_eval)

  if(is.na(chr)){
    cons_tidy <- NA
  } else if(is_num) {
    op_is_eq <- op %in% c('%in%', '==')
    if(length(num_eval) == 1) {
      cons_tidy <- paste0(op,chr_sep)
    } else if (length(num_eval) > 1 & op_is_eq) {
      cons_tidy <- paste0('%in%',chr_sep)
    } else {
      stop("The parameter constraint '", chr, "' is not well defined.")
    }
  } else {
    is_op_chr <- op %in% c('%in%', '==', '!=')
    if(!is_op_chr) {
      stop("Selected boolean operation '", op, "' in '", chr, "' is not applicable to characters.")
    }
    chr_sep <- chr_sep %>%
      str_remove(., rex) %>%
      str_remove_all(., 'c\\(|\\)') %>%
      trimws(.) %>%
      str_split(., '[:space:]*,+[:space:]*| [:space:]+', simplify = T) %>%
      str_remove_all(., "\'|\"") %>%
      paste0("'",., "'")

    if(length(chr_sep) == 1) {
      cons_tidy <- paste0(op, chr_sep)
    } else {
      cons_tidy <- paste0('%in% ', 'c(', paste(chr_sep, collapse = ","), ')')
    }
  }

  return(cons_tidy)
}

#' Translate the unit sequences to calibration.cal syntax
#'
#' @param val_seq Numeric vector with the unit values
#'
#' @keywords internal
#'
translate_sequence <- function(val_seq) {
  if (length(val_seq) > 1) {
    val_seq <- c(min(val_seq), -max(val_seq))
  }
  return(val_seq)
}


#' Concatenate with an underscore
#'
#' \%_\% pastes two strings by "_".
#' @keywords internal
"%_%" <- function(a, b) paste(a, b, sep = "_")

#' Paste slash function
#'
#' \%//\% pastes two strings by "/".
#' @keywords internal
'%//%' <- function(a, b) paste(a, b, sep = "/")

#' Concatenate without separator
#'
#' \%&\% pastes two strings by "".
#' @keywords internal
'%&%' <- function(a, b) paste0(a, b)

#' Concatenate with space
#'
#' \%&&\% pastes two strings by " ".
#' @keywords internal
'%&&%' <- function(a, b) paste(a, b, sep = " ")

#' Concatenate with a dot
#'
#' \%.\% pastes two strings by ".".
#' @keywords internal
'%.%' <- function(a, b) paste(a, b, sep = ".")
