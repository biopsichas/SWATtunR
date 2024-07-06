#' Write Calibration File
#'
#' This function arranges and writes a calibration.cal file based on the provided
#' parameter table.
#'
#' @param par A data frame containing parameter information.
#' @param write_path (optional) The path where the calibration file will be written.
#' Default \code{write_path = NULL}, the current working directory will be used.
#' @param i_run (optional) An integer specifying the run number.
#' Default \code{i_run = 1}.
#'
#' @return None, but write out calibration.cal file
#' @importFrom dplyr bind_rows  %>%
#' @importFrom purrr map
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' write_cal_file(my_parameter_table, "path/to/write", 1)
#' }
#' @keywords write

write_cal_file <- function(par, write_path = NULL, i_run = 1){
  if(is.null(write_path)) write_path <- getwd()
  # Arranging the calibration file.
  par_t <- format_swatplus_parameter(par)
  unit_conds <- read_unit_conditions(model_path, par_t)
  cal <- map(1:nrow(par_t$definition),
             ~ par_t$definition[.x,]) %>%
    map(., ~ setup_calibration_cal(.x, unit_conds)) %>%
    bind_rows(.)
  # Write the calibration file.
  write_calibration(write_path, par_t, cal,
                               seq(1, dim(par)[1], 1), i_run)
}




#' Write SWAT+ input file, which has a tabular structure.
#'
#' @param tbl SWAT input table in tibble (data.frame) format.
#' @param file_path Write path of the SWAT+ input file.
#' @param fmt Character vector of format strings to define the print format of
#' each table column.
#'
#' @returns Writes a text file table in the file path.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_int map2_df
#' @importFrom readr write_lines
#' @importFrom stringr str_remove str_replace str_replace_all
#' @export
#' @examples
#' \dontrun{
#' ## Read the hydrology.hyd input file
#' hydrology_hyd <- read_tbl(paste0(model_path, '/hydrology.hyd'))
#' hydrology_hyd$esco <- 1
#' ## Updating hydrology.hyd file
#' hydr_hyd_fmt <- c('%-16s', rep('%12.5f', 14))
#' write_tbl(hydrology_hyd, paste0(model_path, '/hydrology.hyd'), hydr_hyd_fmt)
#' }
#' @seealso \code{\link{read_tbl}}
#' @keywords write

write_tbl <- function(tbl, file_path, fmt) {
  tbl <- map2_df(tbl, fmt, ~ sprintf(.y, .x))

  fmt_names <- fmt %>%
    str_remove(., '\\.[:digit:]+') %>%
    str_replace(., 'f|d', 's')

  col_names <- colnames(tbl) %>%
    sprintf(fmt_names, .) %>%
    paste(., collapse = '  ')

  file_lines <- tbl %>%
    apply(., 1, paste, collapse = '  ') %>%
    str_replace_all(., '  NA', '    ')

  file_head <- paste('SWAT+ input file updated soft calibration at', Sys.time())

  input_file <- c(file_head, col_names, file_lines)

  write_lines(input_file, file_path)
}
