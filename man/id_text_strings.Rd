% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{id_text_strings}
\alias{id_text_strings}
\title{Generate ID Text Strings for Parameter Names}
\usage{
id_text_strings(par, par_groups, hyd)
}
\arguments{
\item{par}{A character vector specifying the parameter name. Example 'perco'.}

\item{par_groups}{A vector of groups identified based on parameter value.
Example: init_perco <- c(low = 0.01, mod = 0.50, high = 0.95)}

\item{hyd}{A data frame containing 'hydrology.hyd' data.}
}
\value{
A vector of HRU ID text strings separated into groups according to
provided values.
}
\description{
This function generates ID text strings to be included in parameter names based on
parameter groups and hydrology data.
}
\examples{
\dontrun{
hyd_hyd <- read_tbl(paste0(model_path, '/hydrology.hyd'))
init_perco <- c(low = 0.01, mod = 0.50, high = 0.95)
id_text_strings('perco', init_perco, hyd_hyd)
}
}
