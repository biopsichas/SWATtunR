% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{run_to_id}
\alias{run_to_id}
\title{Convert run_names to integer ID values}
\usage{
run_to_id(run_names)
}
\arguments{
\item{run_names}{Character vector providing the run names}
}
\value{
An integer vector with the run IDs
}
\description{
Convert run_names to integer ID values
}
\examples{
\dontrun{
run_sel <- paste0('run_', sprintf('\%04d', c(1,36, 598, 2311)))
run_to_id(run_sel)
}

}
