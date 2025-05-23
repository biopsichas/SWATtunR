% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{calc_fdc_i}
\alias{calc_fdc_i}
\title{Calculate Flow Duration Curve (FDC) for list element i}
\usage{
calc_fdc_i(x)
}
\arguments{
\item{x}{A vector or a tibble with flow values.}
}
\value{
a tibble with sorted values and their corresponding exceedance
probabilities.
}
\description{
Calculate the flow duration curve for a given vector or dataframe.
}
\keyword{internal}
