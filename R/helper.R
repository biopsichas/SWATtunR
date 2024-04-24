#' Add a running ID to duplicated names
#'
#' @param col_name Character vector of column names
#'
#' @returns the `col_name` character vector with IDs for duplicated names
#'
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
