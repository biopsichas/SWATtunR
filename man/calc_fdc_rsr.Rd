% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{calc_fdc_rsr}
\alias{calc_fdc_rsr}
\title{Calculate RSR for Flow Duration Curve Segments}
\usage{
calc_fdc_rsr(fdc_sim, fdc_obs, quantile_splits, out_tbl = "long")
}
\arguments{
\item{fdc_sim}{a tibble with simulated flow data.}

\item{fdc_obs}{a tibble with observed flow data.}

\item{quantile_splits}{a numeric vector with quantiles for splitting the FDC.}

\item{out_tbl}{character specifying the output format ('long' or 'wide').
Default \code{out_tbl = 'long'}.}
}
\value{
a tibble with RSR values for the different segments of the FDC.
}
\description{
This function calculates the ratio of RMSE and standard deviation for
different segments of the flow duration curve (FDC).
}
\examples{
\dontrun{
fdc_sim <- calc_fdc(runif(100))
fdc_obs <- calc_fdc(runif(100))
rsr_values <- calc_fdc_rsr(fdc_sim, fdc_obs, c(5, 20, 70, 95))
}

}
\keyword{helper}
