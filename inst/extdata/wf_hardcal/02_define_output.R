#==============================================================================#
# Step 2: Define the simulation outputs which are returned in the SWAT runs
#
# This is a template and must be adjusted to the individual SWAT project
#==============================================================================#

# Load required R packages ------------------------------------------------
library(SWATrunR)

# Parameter definition ----------------------------------------------------
# Channel IDs for which simulation outputs are returned.
cha_ids <- c(1, 5, 7)
# HRU IDs for which simulation outputs are returned. E.g. all HRUs
hru_ids <- 1:1225

# Output definition -------------------------------------------------------
outputs <- list(
  # Daily discharge
  flo_day = define_output(file = 'channel_sd_day',
                          variable = 'flo_out',
                          unit = cha_ids),
  # Daily in-stream Nitrogen
  no3_day = define_output(file = 'channel_sd_day',
                          variable = 'no3_out',
                          unit = cha_ids),
  orgn_day = define_output(file = 'channel_sd_day',
                           variable = 'orgn_out',
                           unit = cha_ids),
  nh3_day = define_output(file = 'channel_sd_day',
                          variable = 'nh3_out',
                          unit = cha_ids),
  no2_day = define_output(file = 'channel_sd_day',
                          variable = 'no2_out',
                          unit = cha_ids),
  # Daily in-stream phosphorous
  solp_day = define_output(file = 'channel_sd_day',
                          variable = 'solp_out',
                          unit = cha_ids),
  sedp_day = define_output(file = 'channel_sd_day',
                           variable = 'sedp_out',
                           unit = cha_ids),
  # Daily in-stream sediment loads
  sed_day = define_output(file = 'channel_sd_day',
                          variable = 'sed_out',
                          unit = cha_ids),
  # Average annual sediment yields for all HRUs
  sedyld = define_output(file = 'hru_ls_aa',
                         variable = 'sedyld',
                         unit = hru_ids)
)
