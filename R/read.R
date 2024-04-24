#' Read a SWAT+ input file which has a tabular structure.
#'
#' This function reads a SWAT+ input file with a tabular structure into a tibble.
#'
#' @param file_path Path of the SWAT+ input file.
#' @param col_names (optional) A character vector specifying column names.
#' If not provided, column names are inferred from the file.
#' Default \code{col_names = NULL}.
#' @param n_skip (optional) Number of header rows to skip.
#' Default \code{n_skip = 1}.
#'
#' @returns A tibble representing the SWAT+ input file.
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>%
#' @importFrom tibble add_column tibble
#' @export
#'

read_tbl <- function(file_path, col_names = NULL, n_skip = 1) {
  if (file.exists(file_path)) {
    tbl <- fread(file_path, skip = n_skip + 1, header = FALSE)
    if (is.null(col_names)) {
      col_names <- fread(file_path, skip = n_skip, nrows = 1, header = F) %>%
        unlist(.) %>%
        unname(.) %>%
        add_suffix_to_duplicate(.)
    }
    if ('description' %in% col_names & ncol(tbl) == length(col_names) - 1) {
      tbl <- add_column(tbl, description = '')
    } else if (ncol(tbl) > length(col_names)) {
      col_names_add <- paste0('v_', 1:(ncol(tbl) - length(col_names)))
      col_names <- c(col_names, col_names_add)
      warning("Number of columns of '", basename(file_path),"' > column names.\n",
              "Column names ", paste(col_names_add, collapse = ', '),
              ' were assigned to columns at the end.')
    } else if (ncol(tbl) < length(col_names)) {
      col_names_rmv <- col_names[(ncol(tbl) + 1):length(col_names)]
      col_names <- col_names[1:ncol(tbl)]
      warning("Number of columns of '", basename(file_path),"' < column names.\n",
              "Column names ", paste(col_names_rmv, collapse = ', '),
              ' were removed.')
    }

    names(tbl) <- col_names
    tbl <- tibble(tbl)
  } else {
    if (is.null(col_names)) {
      stop("File '", basename(file_path), "' does not exist and no 'col_names' ",
           'were provided to generate empty table.')
    }

    tbl <- tibble(!!!rep(NA, length(col_names)),
                  .rows = 0, .name_repair = ~ col_names)
  }

  return(tbl)
}


