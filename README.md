Introduction to SWATtunR
================

# SWATtunR

[![](https://img.shields.io/badge/devel%20version-0.0.1.9012-blue.svg)](https://github.com/biopsichas/SWATtunR)
[![](https://img.shields.io/github/last-commit/biopsichas/SWATtunR.svg)](https://github.com/biopsichas/SWATtunR/commits/green)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/github/languages/code-size/biopsichas/SWATtunR.svg)](https://github.com/biopsichas/SWATtunR)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)

The goal of `SWATtunR` is to help with the [SWAT+
model](https://swat.tamu.edu/software/plus/) calibration and validation.
These functions were developed and tested for the implementation of
modeling tasks in the [OPTAIN project](https://www.optain.eu/),
[Nordbalt-Ecosafe](https://projects.au.dk/nordbalt-ecosafe) and [LIFE
SIP
Vanduo](https://webgate.ec.europa.eu/life/publicWebsite/project/LIFE22-IPE-LT-LIFE-SIP-Vanduo-101104645/integrated-water-management-in-lithuania).
Functions are initially developed by [Christoph
Schuerz](https://www.ufz.de/index.php?en=49467), which added important
capability on top of other R tools designed for the SWAT/SWAT+ models.
Therefore, we highly recommend trying and using these tools:

- [SWATbuildR](https://git.ufz.de/optain/wp4-integrated-assessment/swat/bildr_script)[^1] -
  R tool for building SWAT+ setups;
- [SWATprepR](https://biopsichas.github.io/SWATprepR/) - SWAT+ model
  input data preparation helper.
- [SWATfarmR](http://chrisschuerz.github.io/SWATfarmR/) - R tool for
  preparing management schedules for SWAT model;
- [SWATdoctR](https://git.ufz.de/schuerz/swatdoctr) - A collection of
  functions in R and routines for SWAT model calibration and model
  diagnostics;
- [SWATrunR](https://chrisschuerz.github.io/SWATrunR/) - R tool for
  running SWAT models for different parameters and scenarios. Please
  install branch names *remove_legacy*. It could be done using line like
  this `remotes::install_github("chrisschuerz/SWATrunR@remove_legacy")`
- [SWATmeasR](https://nc.ufz.de/s/KA9Cr2bbtALGMHr?path=%2FWPs%20%26%20Tasks%2FWP4%2FTask%204.4%2FTools%20to%20share)[^2] -
  R tool for implementing Natural/Small Water Retention Measures
  (NSWRMs) in the SWAT+ models and running scenarios.

<img src="man/figures/swativerse_update.png" title="SWAT packages for R" alt="swativerse logo" width="80%" style="display: block; margin: auto;" />

Detailed information about packages, workflow steps, input data, SWAT+
parameters, model calibration, validation, etc., could be found in the
[SWAT+ modeling protocol](https://doi.org/10.5281/zenodo.7463395).

## Installation

You can install the development version of **SWATtunR** from
[GitHub](https://github.com/biopsichas/SWATtunR).

``` r
# If the package 'remotes' is not installed run first:
install.packages("remotes")

# The installation of `SWATprepR`.
remotes::install_github("biopsichas/SWATtunR")
```

Text text text

<img src="man/figures/eu.png" title="EU LIFE+ logo" alt="eu logo" width="40%" align="left" style="display: block; margin: auto;" />

[^1]: For access please inquire at *<christoph.schuerz@ufz.de>*.

[^2]: For access please inquire at *<christoph.schuerz@ufz.de>*.
