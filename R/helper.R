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
