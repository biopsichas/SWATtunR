Introduction to SWATtunR
================

# SWATtunR

[![](https://img.shields.io/badge/devel%20version-0.0.1.9014-blue.svg)](https://github.com/biopsichas/SWATtunR)
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
  input data preparation helper. The package is presented in the article
  Plunge, Szabó, et al. (2024);
- [SWATfarmR](http://chrisschuerz.github.io/SWATfarmR/) - R tool for
  preparing management schedules for SWAT model;
- [SWATdoctR](https://git.ufz.de/schuerz/swatdoctr) - A collection of
  functions in R and routines for SWAT model calibration and model
  diagnostics. The package is presented in the article Plunge, Schürz,
  et al. (2024);
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
[SWAT+ modeling protocol](https://doi.org/10.5281/zenodo.7463395)
Christoph et al. (2022).

## Installation

You can install the development version of **SWATtunR** from
[GitHub](https://github.com/biopsichas/SWATtunR).

``` r
# If the package 'remotes' is not installed run first:
install.packages("remotes")

# The installation of `SWATprepR`.
remotes::install_github("biopsichas/SWATtunR")
```

<p style="font-family: times, serif; font-size:11pt; font-style:italic">
This work was carried out within the OPTAIN project (OPtimal strategies
to retAIN and re-use water and nutrients in small agricultural
catchments across different soil-climatic regions in Europe,
cordis.europa.eu) which has received funding from the European Union’s
Horizon 2020 research and innovation programme under grant agreement
No. 862756 and the LIFE22-IPE-LT-LIFE-SIP-Vanduo project (Integrated
water management in Lithuania, ref: LIFE22-IPE-LT-LIFE-SIPVanduo/
101104645, cinea.ec.europa.eu), funded by the European Union LIFEprogram
under the grant agreement No 101104645.
</p>

<img src="man/figures/eu.png" title="EU LIFE+ logo" alt="eu logo" width="40%" align="left" style="display: block; margin: auto;" />
<img src="man/figures/optain.png" title="OPTAIN logo" alt="eu logo" width="30%" align="right" style="display: block; margin: auto;" />

<br><br><br>

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-optain2022" class="csl-entry">

Christoph, Schürz, Čerkasova Natalja, Farkas Csilla, Nemes Attila,
Plunge Svajunas, Strauch Michael, Szabó Brigitta, and Piniewski Mikołaj.
2022. “<span class="nocase">SWAT+ modeling protocol for the assessment
of water and nutrient retention measures in small agricultural
catchments</span>.” Zenodo. <https://doi.org/10.5281/zenodo.7463395>.

</div>

<div id="ref-plunge2024a" class="csl-entry">

Plunge, Svajunas, Christoph Schürz, Natalja Čerkasova, Michael Strauch,
and Mikołaj Piniewski. 2024. “<span class="nocase">SWAT+ model setup
verification tool: SWATdoctR</span>.” *Environmental Modelling &
Software* 171: 105878. <https://doi.org/10.1016/j.envsoft.2023.105878>.

</div>

<div id="ref-plunge2024b" class="csl-entry">

Plunge, Svajunas, Brigitta Szabó, Michael Strauch, Natalja Čerkasova,
Christoph Schürz, and Mikołaj Piniewski. 2024.
“<span class="nocase">SWAT + input data preparation in a scripted
workflow: SWATprepR</span>.” *Environmental Sciences Europe* 36 (1): 53.
<https://doi.org/10.1186/s12302-024-00873-1>.

</div>

</div>

[^1]: For access please inquire at *<christoph.schuerz@ufz.de>*.

[^2]: For access please inquire at *<christoph.schuerz@ufz.de>*.
