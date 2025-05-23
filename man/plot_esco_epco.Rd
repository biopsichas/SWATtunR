% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_esco_epco}
\alias{plot_esco_epco}
\title{Plot esco (and epco) to identify appropriate parameter ranges.}
\usage{
plot_esco_epco(sim, wyr_target, rel_wyr_limit = 0.05)
}
\arguments{
\item{sim}{The simulation results water balance data. Including:
\describe{
\item{precip}{Variable out of 'basin_wb_aa' output file.}
\item{surq_cha}{Variable out of 'basin_wb_aa' output file.}
\item{surq_res}{Variable out of 'basin_wb_aa' output file.}
\item{latq_cha}{Variable out of 'basin_wb_aa' output file.}
\item{latq_res}{Variable out of 'basin_wb_aa' output file.}
\item{qtile}{Variable out of 'basin_wb_aa' output file.}
\item{flo}{Variable out of ''basin_aqu_aa' output file.}
}}

\item{wyr_target}{A numeric value representing the target water yield ratio.}

\item{rel_wyr_limit}{A numeric value specifying the relative range for
acceptable error.}
}
\value{
A ggplot object.
}
\description{
Dependent on whether esco (and epco) were used in the calibration runs,
\code{plot_esco_epco()} plots the simulatated water yield ratios over the
parameter ranges to support the manual selection of parameter values/ranges.
}
