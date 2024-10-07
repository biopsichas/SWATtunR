# -------------------------------------------------------------------------
# Step 1: Define the parameter set which is used in the SWAT simulations
#
# This is a template and must be adjusted to the individual SWAT project
# -------------------------------------------------------------------------

# Load required R packages ------------------------------------------------
library(SWATrunR)

# Parameter definition ----------------------------------------------------
# Number parameter combinations
n_combinations <- 1000

# Define parameter boundaries ---------------------------------------------
# Suggested list and ranges of SWAT+ parameters for the calibration of
# different output variables.
# Parameters are roughly divided into groups by processes which they affect most.
# Some groups should be considered optional.
parameter_boundaries <- tibble(
  # snow (optional - use if average snow fall to precipitation ratio is
  # higher than 5%)
  'snomelt_tmp.hru | change = absval' = c(-1.5, 1.5),
  'snofall_tmp.hru | change = absval' = c(-1.5, 1.5),
  'snomelt_lag.hru | change = absval' = c(0, 1),
  'snomelt_min.hru | change = absval' = c(1, 3.5),
  'snomelt_max.hru | change = absval' = c(3.5, 7),
  # ET (note: it is suggested that a narrow range for esco selected in soft
  # calibration of water balance is used instead of the wide (0,1) range)
  'esco.hru | change = absval' = c(0.05, 1),
  'epco.hru | change = absval' = c(0.05, 1),
  'awc.sol | change = relchg' = c(-0.25, 0.25),
  #surface runoff
  'cn2.hru | change = relchg' = c(-0.15, 0.10),
  'cn3_swf.hru | change = absval' = c(0.05, 0.95),
  'ovn.hru | change  = relchg ' = c(-0.25, 0.25),
  'surlag.bsn | change = absval' = c(0.005, 4),
  # lateral flow (optional - use if lateral flow constitutes at least 5% of
  # total water yield)
  'lat_len.hru | change = abschg' = c(-20, 20),
  'latq_co.hru | change = absval' = c(0.05, 0.95),
  'bd.sol | change = relchg' = c(-0.35, 0.35),
  'k.sol | change = relchg' = c(-0.5, 2),
  # tile flow (optional - use if tile flow constitutes at least 5% of total
  # water yield; note: tile_lag and tile_dtime should be active only if
  # tile_drain is set to 0 in codes.bsn file))
  'tile_dep.hru | change = relchg' = c(0.1, 0.3),
  'tile_lag.hru | change = absval' = c(20, 25),
  'tile_dtime.hru | change = absval' = c(50, 100),
  # percolation/aquifer
  'perco.hru | change = absval' = c(0.05, 0.95),
  'flo_min.aqu | change = abschg' = c(-2, 2),
  'revap_co.aqu | change = absval' = c(0.02, 0.2),
  'revap_min.aqu | change = abschg' = c(-2, 2),
  'alpha.aqu | change = absval' = c(0.001, 0.1),
  'sp_yld.aqu | change = absval' = c(0.001, 0.05),
  'bf_max.aqu | change = absval' = c(0.5, 2),
  # channel routing
  'chn.rte | change = absval' = c(0.02, 0.3),
  'chs.rte | change = relchg' = c(-0.2, 0.2),
  # sediment routing
  'cov.rte | change = absval' = c(0.005, 1),
  'bedldcoef.rte | change = absval' = c(0.01, 1),
  'cherod.rte | change = absval' = c(0.05, 1),
  # nitrogen parameters from here
  "n_updis.bsn | change = absval" = c(10, 80),
  "nperco.bsn | change = absval" = c(0.5, 1),
  "sdnco.bsn | change = absval" = c(0.75, 0.9),
  "hlife_n.aqu | change = absval" = c(0, 200),
  "no3_init.aqu | change = absval" = c(0, 30),
  "cmn.bsn | change = absval" = c(0.001, 0.0013),
  "rsdco.bsn | change = absval" = c(0.02, 0.1)
  )

# Sample parameter combinations -------------------------------------------
parameter_set <- sample_lhs(parameter_boundaries, n_combinations)
