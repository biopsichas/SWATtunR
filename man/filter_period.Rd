% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{filter_period}
\alias{filter_period}
\title{Filter a time period for a variable time series table.}
\usage{
filter_period(tbl, time_window)
}
\arguments{
\item{tbl}{Variable time series table, were one column is of type date and
all other columns are numeric values}

\item{time_window}{Vector of length 2 which provides the start and end dates
of the time window to be filtered. Must be in any format such as
\code{c(2000, 2010)}, or \code{c(20000101, 20101231)}, or e.g.
\code{c('2000-01-01', '2010-12-31')}}
}
\value{
The table \code{tbl} with the filtered rows based on the \code{time_window}.
}
\description{
\code{filter_period()} filters rows from a table which has one date column, where
the dates are in the time range within the \code{time_window}.
}
\examples{
\dontrun{
flow_sim <- filter_period(tbl = flow_sim, time_window = c(2010, 2015))
}
}
\keyword{data}
\keyword{manipulation}
