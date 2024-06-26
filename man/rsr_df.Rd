% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{rsr_df}
\alias{rsr_df}
\title{Calculate RSR for Dataframe Segments}
\usage{
rsr_df(df_sim, v_obs)
}
\arguments{
\item{df_sim}{a dataframe with simulated values.}

\item{v_obs}{a dataframe with observed values.}
}
\value{
a numeric vector with RSR values for each segment.
}
\description{
This function calculates the RSR values for the different segments of the data.
}
\examples{
\dontrun{
rsr_values <- rsr_df(df_sim, v_obs)
}
}
\keyword{internal}
