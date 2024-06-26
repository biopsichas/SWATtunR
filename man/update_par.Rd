% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{update_par}
\alias{update_par}
\title{Update Parameters}
\usage{
update_par(par, par_up, change)
}
\arguments{
\item{par}{Numeric vector: Original parameter values.}

\item{par_up}{Numeric vector: Values indicating the update for each parameter.}

\item{change}{Character scalar: Type of change to apply. Options are "relchg" for relative change,
"pctchg" for percentage change, "abschg" for absolute change, and "absval" for setting the absolute value change.}
}
\value{
Numeric vector with updated parameter values.
}
\description{
This function updates parameters based on different types of changes.
}
\examples{
par <- c(10, 20, 30, 40)
par_up <- c(0.1, 0.2, NA, 0.3)
update_par(par, par_up, "relchg") # Returns updated parameters with relative change applied.

}
\keyword{helper}
