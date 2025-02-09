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
