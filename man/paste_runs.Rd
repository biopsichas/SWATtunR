% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{paste_runs}
\alias{paste_runs}
\title{Combine Start and End Values into a Range String}
\usage{
paste_runs(strt, end, sep)
}
\arguments{
\item{strt}{The starting value of the range.}

\item{end}{The ending value of the range.}

\item{sep}{A string separator to use between the start and end values. Default is ':'.}
}
\value{
A string representing the range. If the start and end values are the same,
the single value is returned. Otherwise, the start and end values are concatenated
with the specified separator.
}
\description{
This function creates a string representation of a range by combining start
and end values. If the start and end values are the same, it returns the
single value as a string. Otherwise, it concatenates them with a separator.
}
\keyword{internal}
