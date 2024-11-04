# -------------------------------------------------------------------------
# Step 1: Define the parameter set which is used in the SWAT simulations
#
# This is a template and must be adjusted to the individual SWAT project
# -------------------------------------------------------------------------

# Load required R packages ------------------------------------------------
library(SWATtunR)
library(tibble)

# Parameter definition ----------------------------------------------------
# Number parameter combinations
n_combinations <- 100

# Path to the SWAT+ project folder.
model_path <- ''

model_path <- 'C:/TxtInOut/'

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
  'cn3_swf.hru | change = absval' = c(0, 1), # Normalized range. Will be updated below
  'ovn.hru | change  = relchg ' = c(-0.25, 0.25),
  'surlag.bsn | change = absval' = c(0.005, 4),
  # lateral flow (optional - use if lateral flow constitutes at least 5% of
  # total water yield)
  'lat_len.hru | change = abschg' = c(-20, 20),
  'latq_co.hru | change = absval' = c(0, 1), # Normalized range. Will be updated below
  'bd.sol | change = relchg' = c(-0.35, 0.35),
  'k.sol | change = relchg' = c(-0.5, 2),
  # tile flow (optional - use if tile flow constitutes at least 5% of total
  # water yield; note: tile_lag and tile_dtime should be active only if
  # tile_drain is set to 0 in codes.bsn file))
  'tile_dep.hru | change = relchg' = c(0.1, 0.3),
  'tile_lag.hru | change = absval' = c(20, 25),
  'tile_dtime.hru | change = absval' = c(50, 100),
  # percolation/aquifer
  'perco.hru | change = absval' = c(0, 1), # Normalized range. Will be updated below
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

# Apply different ranges to cn3_swf, latq_co, and perco ------------------
# This step is optional and should be removed if you do not want to consider
# different parameter ranges to the different intial values of the three
# parameters cn3_swf, latq_co, and perco.
# By default different initial values are assigned to these three parameters
# during model setup (the initial values are based on the runoff and leaching
# potentials of the respective HRUs). In order not to loose this spatial
# information in the parameters the calibration of those parameters should
# be done with different ranges applied for the different initial values
# of a parameter.
# The following routine accounts for that and generates multiple parameter
# columns from one parameter column which was sampled above. Take care that
# the parameter was sampled in the range 0 to 1 above to be translated into
# the different ranges below. Range can also be smaller if e.g. range was
# narrowed down in the calibration process.

# Separate the HRUs of the SWAT+ model setup into groups based on their initial
# values of e.g. perco, cn3_swf, and latq_co
perco_groups <- group_hydr_values('perco',   model_path)
cn3_groups   <- group_hydr_values('cn3_swf', model_path)
latq_groups  <- group_hydr_values('latq_co', model_path)

# Define parameter boundaries in which the parameter values should vary for
# the different initial values.
# The list can be a named list, the names defined here will be added to the
# parameter names for differentiation. If unnamed the indexes _1, _2, and _3
# will be added to the parameter names instead.
perco_bound <- list(low = c(0.05, 0.30), mod = c(0.30, 0.60), high = c(0.50, 0.95))
cn3_bound   <- list(low = c(0.0, 0.30), mod = c(0.15, 0.45), high = c(0.50, 0.95))
latq_bound  <- list(low = c(0.01, 0.30), mod = c(0.10, 0.40), high = c(0.5, 0.90))

# Add the additional parameter columns to the parameter table and remove the
# column with the respective normalized parameter values.
parameter_set <- translate_to_boundaries(par_tbl = parameter_set,
                                         par_name = 'perco.hru | change = absval',
                                         par_bound = perco_bound,
                                         par_group = perco_groups)

parameter_set <- translate_to_boundaries(par_tbl = parameter_set,
                                         par_name = 'cn3_swf.hru | change = absval',
                                         par_bound = cn3_bound,
                                         par_group = cn3_groups)

parameter_set <- translate_to_boundaries(par_tbl = parameter_set,
                                         par_name = 'latq_co.hru | change = absval',
                                         par_bound = latq_bound,
                                         par_group = latq_groups)
