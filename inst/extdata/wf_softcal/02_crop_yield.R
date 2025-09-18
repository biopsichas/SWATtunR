# -------------------------------------------------------------------------
# Step 2: Crop soft calibration (yields)
#
# This is a template and must be adjusted to the individual SWAT project.
#
# This is a template that must be adjusted for the specific SWAT project.
#
# It provides a routine for calibrating crop parameters using observed crop yields.
#
# The calibration is a two-stage process. This script represents the second stage,
# where additional crop parameters can be fine-tuned if adjusting the days to maturity
# alone is not sufficient to match observed yields.
#
# IMPORTANT: The `dmat_sel` object must be present in the environment,
# carried over from the first crop calibration step (01_crop_phu.R).
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
             save_file        = add_timestamp('sim_yld'),
             return_output    = FALSE,
             time_out         = 3600 # seconds, change if run-time differs
             )

# Load the most recent yield simulation results
yld_sims <- list.files('./simulation/', pattern = '[0-9]{12}_sim_yld')
yld_path <- paste0('./simulation/', yld_sims[length(yld_sims)])
yld_sim  <- load_swat_run(yld_path)
# Remove days to maturity parameter columns before plotting.
yld_sim$parameter$values <- yld_sim$parameter$values[, 1:ncol(par_bnd)]

## Plot dotty figures for the selected crops
plot_dotty_yields(yld_sim, yield_obs)

# Fix the parameter changes you want to apply to the crops
crop_par_sel <- tibble(
  plant_name                       = c("corn", "cots", "pnut"),
  'bm_e.pdb | change = relchg'     = c(  -0.2,   -0.3,   0.1),
  'harv_idx.pdb | change = relchg' = c(  -0.15,  -0.3,   0.3),
  'lai_pot.pdb | change = relchg'  = c(  -0.2,   -0.3,   0.3),
  'tmp_base.pdb | change = abschg' = c(   1.5,    1.5,  -1.0))

# Check if user defined days to maturity values for all crops.
stopifnot(all(crop_names %in% crop_par_sel$plant_name))
# Restructure the set parameter changes to SWATrunR
crop_par_sel <- prepare_plant_parameter(crop_par_sel)

# Final simulation runs to check PHUs and yields --------------------------
# Build parameter table with selected days to maturity and selected crop
# parameter values
par_final <- bind_cols(dmat_sel, crop_par_sel)

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
             save_file        = add_timestamp('sim_check01'),
             return_output    = FALSE,
             time_out         = 3600, # seconds, change if run-time differs
             keep_folder      = TRUE
)

# Load the most recent check simulation results
check_sims <- list.files('./simulation/', pattern = '[0-9]{12}_sim_check01')
check_path <- paste0('./simulation/', check_sims[length(check_sims)])
check_sim  <- load_swat_run(check_path)

# Plot PHU, crop yields and biomass for final simulation run.
plot_phu_yld_bms(check_sim, yield_obs)

# Write plants.plt --------------------------------------------------------
# If final run looks acceptable, write the adjusted parameter table into the
# project folder. You have to set overwrite = TRUE below.
# This overwrites the original plants.plt!
# The original plants.plt is available as a backup from ./backup/plants.plt
file.copy(paste0(model_path, '/.model_run/thread_1/plants.plt'), model_path,
          overwrite = FALSE)
unlink(paste0(model_path, '/.model_run'), recursive = TRUE)
