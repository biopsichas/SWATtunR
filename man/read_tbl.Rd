% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_tbl}
\alias{read_tbl}
\title{Read a SWAT+ input file which has a tabular structure.}
\usage{
read_tbl(file_path, col_names = NULL, n_skip = 1)
}
\arguments{
\item{file_path}{Path of the SWAT+ input file.}

\item{col_names}{(optional) A character vector specifying column names.
If not provided, column names are inferred from the file.
Default \code{col_names = NULL}.}

\item{n_skip}{(optional) Number of header rows to skip.
Default \code{n_skip = 1}.}
}
\value{
A tibble representing the SWAT+ input file.
}
\description{
This function reads a SWAT+ input file with a tabular structure into a tibble.
}
\keyword{internal}
