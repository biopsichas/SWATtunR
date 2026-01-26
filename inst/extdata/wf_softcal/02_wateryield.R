#==============================================================================#
# Step 2: Water yield soft calibration
#
# This is a template and must be adjusted to the individual SWAT project.
#
# The template provides a routine to calibrate the parameter esco to meet a
# water yield ratio of the modeled catchment (Alternative A below). Optionally,
# the parameters esco and epco can be calibrated together (Alternative B). Be
# aware that the parameter epco is/was hard coded in some revisions of SWAT+
# (see issue in https://github.com/swat-model/swatplus/issues/28). In such case
# the calibration of epco is pointless.
#==============================================================================#

# Load required R packages ------------------------------------------------
library(SWATtunR)
library(SWATrunR)
library(tibble)

# Parameter definition ----------------------------------------------------
# Decide for calibration alternative 'A' (only esco) or 'B' (esco and epco).
alternative <- 'B'

# Path to the SWAT+ project folder.
model_path <- ''

# Set the number of cores for parallel model execution
n_cores <- Inf # Inf uses all cores. Set lower value if preferred.

# Set the number of steps in which the parameters esco/epco should be sampled
# A low number of e.g. 5 to 10 is absolutely sufficient.
n_step <- 5

# Target water yield ratio. This value must be provided by user, based e.g. on
# analyses of the discharge hydrograph, or study site specific expert knowledge.
# The value must be provided as a fraction in the range [0,1].
wyr_target <- 0.18

# Load and prepare data ---------------------------------------------------
# Load the yield observations
yield_obs_path <- './observation/crop_yields.csv'
yield_obs  <- read.csv(yield_obs_path)

# Define the crops which should be used in the calibration.
# Default is all crops which are defined in yield_obs.
# Please define manually if only selected crops should be considered.
crop_names <- yield_obs$plant_name

# Optional reset of hydrology.hyd -----------------------------------------
# In the case the water yield ratio calibration workflow should be redone after
# the last step of this script was already executed and the hydrology.hyd was
# overwritten the hydrology.hyd should be reset to its initial condition. To
# perform the reset set reset <- TRUE
reset <- FALSE
if(reset & file.exists('./backup/hydrology.hyd')) {
  file.copy('./backup/hydrology.hyd',
            paste0(model_path, '/hydrology.hyd'),
            overwrite = TRUE)
}

# Parameter calibration ---------------------------------------------------

# Alternative A: Calibrate esco -------------------------------------------
if(alternative == 'A') {
  # Sample the paramter esco with the defined number of steps.
  par_esco_epco <- tibble('esco.hru | change = absval' =
                            seq(0.05,0.95, length.out = n_step))

# Alternative B: Calibrate esco and epco ----------------------------------
} else if (alternative == 'B') {
  # Define the esco and epco parameter ranges.
  par_bnd <- tibble('esco.hru | change = absval' = seq(0.05, 0.95, length.out = n_step),
                    'epco.hru | change = absval' = seq(0.05, 0.95, length.out = n_step))

  # Sample the esco epco combinations with LHS sampling.
  par_esco_epco <- expand.grid(par_bnd)
}

# The SWATrunR package is used to run simulations for the different parameter
# samples. All simulation results will be saved in the folder './simulation'.
# The simulation results will have a time stamp, so if the process is repeated
# always the most recent simulations are used in the analysis.
run_swatplus(project_path = model_path,
             output = list(precip  =  define_output(file = 'basin_wb_aa',
                                                    variable = 'precip',
                                                    unit = 1),
                           surq_cha = define_output(file = 'basin_wb_aa',
                                                    variable = 'surq_cha',
                                                    unit = 1),
                           surq_res = define_output(file = 'basin_wb_aa',
                                                    variable = 'surq_res',
                                                    unit = 1),
                           latq_cha = define_output(file = 'basin_wb_aa',
                                                    variable = 'latq_cha',
                                                    unit = 1),
                           latq_res = define_output(file = 'basin_wb_aa',
                                                    variable = 'latq_res',
                                                    unit = 1),
                           qtile =    define_output(file = 'basin_wb_aa',
                                                    variable = 'qtile',
                                                    unit = 1),
                           flo =      define_output(file = 'basin_aqu_aa',
                                                    variable = 'flo',
                                                    unit = 1)
             ),
             parameter        = par_esco_epco,
             start_date       = NULL, # Change if necessary.
             end_date         = NULL, # Change if necessary.
             add_date         = FALSE,
             years_skip       = NULL, # Change if necessary.
             n_thread         = n_cores,
             save_path        = './simulation',
             save_file        = add_timestamp('sim_wbal'),
             return_output    = FALSE,
             time_out         = 3600 # seconds, change if run-time differs
             )

# Load the most recent simulation results of the water balance components.
wbal_sims <- list.files('./simulation/', pattern = '[0-9]{12}_sim_wbal')
wbal_path <- paste0('./simulation/', wbal_sims[length(wbal_sims)])
wbal_sim  <- load_swat_run(wbal_path, add_date = FALSE)
failed_runs(wbal_sim)

# Select esco/epco parameter values ---------------------------------------
#
# Based on the simulated water balance components for the different esco/epco
# values a simulated water yield ratio is calculated and plotted over the
# parameter values. The target water yield ratio is also shown in the plot to
# support the selection of an appropriate range/value for esco and epco.

plot_esco_epco(wbal_sim, wyr_target, rel_wyr_limit = 0.05)

# USER DECISION STEP -----------------------------------------------------------
#
# The esco/epco plot shows recommended values for the parameters to meet the
# target water yield ratio. There are two options to define esco and epco for
# further model use:
# - Option 1: Writing esco and epco into the file hydrology.hyd
#   If single values are selected for esco and/or epco those can be directly
#   written into the file hydrology.hyd, which overwrites the initial values.
#   In this case the new fixed parameter values can be either the starting
#   point for further calibration or be removed in further calibration steps
#   and fixing the parameters at the selected values.
# - Option 2: Selecting parameter ranges for further use in calibration
#   In this case, parameter ranges are selected from the plots and can e.g.
#   be used in additional SWATrunR simulation runs for hard calibration.
#
# Before deciding which option is preferred, it is recommended to perform an
# additional simulation with a target parameter set for esco/epco to check the
# simulated water yield ratio and the simulated crop yields as e.g. epco can
# affect the simulated plant growth.
#

if (alternative == 'A') {
  par_check <- tibble('esco.hru | change = absval' = 0.95) # Adjust accordingly
} else if (alternative == 'B') {
  par_check <- tibble('esco.hru | change = absval' = 0.95, # Adjust accordingly
                      'epco.hru | change = absval' = 1.00) # Adjust accordingly
}

## Rerun model for crop yields results
run_swatplus(project_path = model_path,
             output = list(precip  =  define_output(file = 'basin_wb_aa',
                                                    variable = 'precip',
                                                    unit = 1),
                           surq_cha = define_output(file = 'basin_wb_aa',
                                                    variable = 'surq_cha',
                                                    unit = 1),
                           surq_res = define_output(file = 'basin_wb_aa',
                                                    variable = 'surq_res',
                                                    unit = 1),
                           latq_cha = define_output(file = 'basin_wb_aa',
                                                    variable = 'latq_cha',
                                                    unit = 1),
                           latq_res = define_output(file = 'basin_wb_aa',
                                                    variable = 'latq_res',
                                                    unit = 1),
                           qtile    = define_output(file = 'basin_wb_aa',
                                                    variable = 'qtile',
                                                    unit = 1),
                           flo      = define_output(file = 'basin_aqu_aa',
                                                    variable = 'flo',
                                                    unit = 1),
                           yld      = define_output(file = 'mgtout',
                                                    variable = 'yld',
                                                    label = crop_names),
                           bms      = define_output(file = 'mgtout',
                                                    variable = 'bioms',
                                                    label = crop_names),
                           phu      = define_output(file = 'mgtout',
                                                    variable = 'phu',
                                                    label = crop_names)
             ),
             parameter        = par_check,
             start_date       = NULL, # Change if necessary.
             end_date         = NULL, # Change if necessary.
             # add_date         = FALSE,
             years_skip       = NULL, # Change if necessary.
             n_thread         = n_cores,
             save_path        = './simulation',
             save_file        = add_timestamp('sim_check02'),
             return_output    = FALSE,
             time_out         = 3600 # seconds, change if run-time differs
)

# Load the most recent simulation results.
check_sims <- list.files('./simulation/', pattern = '[0-9]{12}_sim_check02')
check_path <- paste0('./simulation/', check_sims[length(check_sims)])
check_sim  <- load_swat_run(check_path, add_date = FALSE)

# Plot PHU, crop yields and biomass for final simulation run.
plot_phu_yld_bms(check_sim, yield_obs)

# Check the simulated water yield ratio with reference to the target wyr.
calc_wyr(check_sim)

# Option 1: Re-writing SWAT+ parameter file hydrology.hyd
# If SWATreadR is not installed please install from:
# devtools::install_github('chrisschuerz/SWATreadR')

library(SWATreadR)

if(!file.exists('./backup/hydrology.hyd')) {
  file.copy(paste0(model_path, '/hydrology.hyd'), './backup/hydrology.hyd')
}

hydrology_hyd <- read_swat(paste0(model_path, '/hydrology.hyd'))

hydrology_hyd$esco <- 0.95 # set value
hydrology_hyd$epco <- 1.00 # set value if epco is considered

write_swat(hydrology_hyd, paste0(model_path, '/hydrology.hyd'), overwrite = TRUE)

# Option 2: Setting parameter values for SWATrunR runs
# To further use esco and epco ranges in e.g. the hard calibration workflow
# add esco and epco ranges in the parameter definition (by default they are
# included). To include their full ranges it may look like this.
# Please adjust the ranges based on the results above.
parameter_boundaries <- tibble(
  'esco.hru | change = absval' = c(0.05, 1),
  'epco.hru | change = absval' = c(0.05, 1),
)
