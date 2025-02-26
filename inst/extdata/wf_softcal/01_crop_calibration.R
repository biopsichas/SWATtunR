# -------------------------------------------------------------------------
# Step 1: Crop soft calibration
#
# This is a template and must be adjusted to the individual SWAT project.
#
# The template provides a routine to calibrate crop parameters with reference
# to observed crop yields for the crops.
# The routine is a two staged process, where first the days to maturity of each
# crop is tuned to meet the crop characteristics and the implemented management
# schedules.
# In a second step additional crop parameters can be fine tuned in case yields
# cannot be met with just adjusting the days to maturity of a crop.
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

# Set the umber parameter combinations for the LHS sampling of crop parameters
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
  file.copy('./backup/plants.plt', paste(model_path, '/plants.plt'))
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
dmat_sim_name <- paste0(format(Sys.time(), '%Y%m%d%H%M'), '_sim_dmat')

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
             save_file        = dmat_sim_name,
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

# Calibrate additional crop parameters ------------------------------------
## Define changes to be applied to parameter values of selected crop parameters
## Make sure your updates will not produce unrealistic (e.g. negative) values!
par_bnd <- tibble('lai_pot.pdb | change = relchg'  = c(-0.3, 0.3),
                  'harv_idx.pdb | change = relchg' = c(-0.3, 0.3),
                  'tmp_base.pdb | change = abschg' = c(-1.5, 1.5),
                  'bm_e.pdb | change = relchg'     = c(-0.3, 0.1))

## The number of samples can be adjusted based on the available computational resources.
## Recommended number of samples is 50-100.
par_crop <- sample_lhs(par_bnd, n_combinations)
# Add updated days to maturity values to parameter set
par_crop <- bind_cols(par_crop, dmat_sel)

# All simulation results will be saved in the folder './simulation'.
# The simulation results will have a time stamp, so if the process is repeated
# Always the most recent simulations are used in the analysis.
yld_sim_name <- paste0(format(Sys.time(), '%Y%m%d%H%M'), '_sim_yld')

# Run the simulations
run_swatplus(project_path = model_path,
             output = list(yld = define_output(file = 'mgtout',
                                               variable = 'yld',
                                               label = crop_names)),
             parameter = par_crop,
             start_date       = NULL, # Change if necessary.
             end_date         = NULL, # Change if necessary.
             years_skip       = NULL, # Change if necessary.
             n_thread         = n_cores,
             save_path        = './simulation',
             save_file        = yld_sim_name,
             return_output    = FALSE,
             time_out         = 3600 # seconds, change if run-time differs
             )

# Load the most recent dmat simulation results
yld_sims <- list.files('./simulation/', pattern = '[0-9]{12}_sim_yld')
yld_path <- paste0('./simulation/', yld_sims[length(yld_sims)])
yld_sim  <- load_swat_run(yld_path)
# Remove days to maturity parameter columns before plotting.
yld_sim$parameter$values <- yld_sim$parameter$values[, 1:ncol(par_bnd)]

## Plot dotty figures for the selected crops
plot_dotty_yields(yld_sim, yield_obs)

# Fix the parameter changes you want to apply to the crops
crop_par_sel <- tibble(
  plant_name                       = c("corn", "pnut"),
  'lai_pot.pdb | change = relchg'  = c(  -0.1,    0.1),
  'harv_idx.pdb | change = relchg' = c(  -0.1,    0.0),
  'tmp_base.pdb | change = abschg' = c(   0.0,    0.0),
  'bm_e.pdb | change = relchg'     = c(   0.0,    0.0))

# Check if user defined days to maturity values for all crops.
stopifnot(all(crop_names %in% crop_par_sel$plant_name))
# Restructure the set parameter changes to SWATrunR
crop_par_sel <- prepare_plant_parameter(crop_par_sel)

# Final simulation runs to check PHUs and yields --------------------------
# Build parameter table with selected days to maturity and selected crop
# parameter values
par_final <- bind_cols(dmat_sel, crop_par_sel)

final_sim_name <- paste0(format(Sys.time(), '%Y%m%d%H%M'), '_sim_final')

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
             parameter        = par_final,
             start_date       = NULL, # Change if necessary.
             end_date         = NULL, # Change if necessary.
             years_skip       = NULL, # Change if necessary.
             n_thread         = n_cores,
             save_path        = './simulation',
             save_file        = final_sim_name,
             return_output    = FALSE,
             time_out         = 3600, # seconds, change if run-time differs
             keep_folder      = TRUE
)

# Load the most recent dmat simulation results
final_sims <- list.files('./simulation/', pattern = '[0-9]{12}_sim_final')
final_path <- paste0('./simulation/', final_sims[length(final_sims)])
final_sim  <- load_swat_run(final_path)

# Plot PHU, crop yields and biomass for final simulation run.
plot_phu_yld_bms(final_sim, yield_obs)

# Write plants.plt --------------------------------------------------------
# If final run looks acceptable, write the adjusted parameter table into the
# project folder. You have to set overwrite = TRUE below.
# This overwrites the original plants.plt!
# The original plants.plt is available as a backup from ./backup/plants.plt
file.copy(paste0(model_path, '/.model_run/thread_1/plants.plt'), model_path,
          overwrite = FALSE)
unlink(paste0(model_path, '/.model_run'), recursive = TRUE)
