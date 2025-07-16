# -------------------------------------------------------------------------
# Step 1: Crop Soft Calibration (PHU)
#
# This is a template and must be adjusted for the specific SWAT project.
#
# The template provides a routine for calibrating crop parameters based on
# observed crop yields.
#
# The calibration is a two-stage process. In this first step, the days to maturity
# (PHU – Potential Heat Units) of each crop are adjusted to align with crop
# characteristics and the implemented management schedules.
# -------------------------------------------------------------------------

# Load required R packages ------------------------------------------------
library(SWATtunR)
library(SWATrunR)

library(dplyr)
library(tibble)

# Parameter definition ----------------------------------------------------
# Path to the SWAT+ project folder.
model_path <- ''

# Set the number of cores for parallel model execution
n_cores <- Inf # Inf uses all cores. Set lower value if preferred.

# Set the number parameter combinations for the LHS sampling of crop parameters
n_combinations <- 10

# Path to the observed crop yields.
# This file must be updated with case study specific records!
yield_obs_path <- './observation/crop_yields.csv'

# Load and prepare data ---------------------------------------------------
# Load the yield observations
yield_obs  <- read.csv(yield_obs_path)

# Define the crops which should be used in the calibration.
# Default is all crops which are defined in yield_obs.
# Please define manually if only selected crops should be considered.
crop_names <- yield_obs$plant_name

# Optional reset of plants.plt --------------------------------------------
# In the case the crop calibration workflow should be redone after the last step
# of this script was already executed and the plants.plt was overwritten the
# plants.plt should be reset to its initial condition. To perform the reset set
# reset <- TRUE
reset <- FALSE
if(reset) {
  file.copy('./backup/plants.plt',
            paste0(model_path, '/plants.plt'),
            overwrite = TRUE)
} else if (!file.exists('./backup/plants.plt')){
  file.copy(paste0(model_path, '/plants.plt'),
            './backup/plants.plt',
            overwrite = FALSE)
}

# Calibrate days to maturity values for selected crops --------------------

# The days to maturity (days_mat) define how fast or slow a crop develops. In a
# SWAT+ model run the days_mat is converted in the the required heat units which
# a crop needs to fully mature. To result in an intended behavior of a crop the
# days_mat must be in line with the defined management operations schedule.

# To identify reasonable values for days_mat for the selected crops a parameter
# set is generated where the days_mat value for each crop is changed in a range
# (change_min, change_max) with fixed intervals change_step.
par_dmat <- sample_days_mat(crop_names)

# The SWATrunR package is used to run simulations for the different days_mat
# values and return the PHU fractions, yields and biomasses for the selected
# crops.
# All simulation results will be saved in the folder './simulation'.
# The simulation results will have a time stamp, so if the process is repeated
# Always the most recent simulations are used in the analysis.

# Run the simulations
run_swatplus(project_path = model_path,
             output = list(yld = define_output(file = 'mgtout',
                                               variable = 'yld',
                                               label = crop_names),
                           bms = define_output(file = 'mgtout',
                                               variable = 'bioms',
                                               label = crop_names),
                           phu = define_output(file = 'mgtout',
                                               variable = 'phu',
                                               label = crop_names)
             ),
             parameter        = par_dmat,
             start_date       = NULL, # Change if necessary.
             end_date         = NULL, # Change if necessary.
             years_skip       = NULL, # Change if necessary.
             n_thread         = n_cores,
             save_path        = './simulation',
             save_file        = add_timestamp('sim_dmat'),
             return_output    = FALSE,
             time_out         = 3600 # seconds, change if run-time differs
             )

# Load the most recent dmat simulation results
dmat_sims <- list.files('./simulation/', pattern = '[0-9]{12}_sim_dmat')
dmat_path <- paste0('./simulation/', dmat_sims[length(dmat_sims)])
ylds_phu_dmat <- load_swat_run(dmat_path)

# Plot PHU, crop yields and biomass over adjusted days to maturity values.
plot_phu_yld_bms(ylds_phu_dmat, yield_obs)

# Set days to maturity values for all selected crops based on the figure above.
dmat_sel <- tibble(
  plant_name                       = c('corn', 'pnut'),
  'days_mat.pdb | change = absval' = c(  150,     150))

# Check if user defined days to maturity values for all crops.
stopifnot(all(crop_names %in% dmat_sel$plant_name))
# Update names of dmat_sel to be used as SWATrunR parameters
dmat_sel <- prepare_plant_parameter(dmat_sel)
