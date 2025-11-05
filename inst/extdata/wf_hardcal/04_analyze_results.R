# -------------------------------------------------------------------------
# Step 4: Analyze simulation results
# -------------------------------------------------------------------------

# Load required R packages ------------------------------------------------
library(SWATrunR)
library(SWATtunR)
library(hydroGOF)
library(tidyverse)

# Parameter definition ----------------------------------------------------
# Paths to simulation and observation data
# E.g. load the simulations with the last time stamp, if default
# save_file names in simulation runs is used.
sims <- list.files('./simulation/', pattern = '[0-9]{12}')
sim_path <- paste0('./simulation/', sims[length(sims)])

# E.g. path to discharge observations
flow_path <- './observation/q.csv'
# E.g. path to in-stream nitrate observations
ntot_path <- './observation/ntot.csv'

# Define time period to analyze (E.g. calibration or validation period)
# Periods for different variables can be different, but calibration and
# validation should not be mixed.
# Discharge
period_q <- c('2015-01-01', '2017-12-31')
# Water quality
period_wq <- c('2016-01-01', '2017-12-31')

# Load and prepare simulation results -------------------------------------
sim <- load_swat_run(sim_path)

# Extract parameter set
par_vals <- sim$parameter$values

# Extract relevant output variables
# E.g. discharge
flow_sim <- filter_period(sim$simulation$flo_day, period_q)
# E.g. N load components
no3_sim  <- filter_period(sim$simulation$no3_day, period_wq)
nh3_sim  <- filter_period(sim$simulation$nh3_day, period_wq)
no2_sim  <- filter_period(sim$simulation$no2_day, period_wq)
orgn_sim <- filter_period(sim$simulation$orgn_day, period_wq)

# E.g. if FDC segments should be evaluated, calculate the
# FDC for the simulated discharges
flow_fdc_sim <- calc_fdc(flow_sim)


# Calculate total N from the four N fractions
ntot_sim <- sum_variables(no3_sim, nh3_sim, no2_sim, orgn_sim)

# Convert loads to concentrations
ntot_conc_sim <- load_to_conc(load = ntot_sim, flow = flow_sim,
                              load_unit = 'kg day-1', flow_unit = 'm3 s-1',
                              out_unit  = 'mg L-1')


# Load and prepare observations -------------------------------------------
# E.g. discharge observations
flow_obs <- read_csv(flow_path) %>%
  filter_period(., period_q)
# E.g. Ntot concentration observations
ntot_conc_obs <- read_csv(ntot_path) %>%
  filter_period(., period_q)

# E.g. if FDC segments should be evaluated, calculate the
# FDC for the simulated discharges
flow_fdc_obs <- calc_fdc(flow_obs)

# E.g. if calibration should be done with loads and not concentrations
# convert observed concentrations to loads
ntot_obs <- conc_to_load(conc = ntot_conc_obs, flow = flow_obs,
                         conc_unit = 'mg L-1', flow_unit = 'm3 s-1',
                         out_unit  = 'kg day-1')

# Calculate goodness-of-fit values ----------------------------------------
# E.g. to calculate typical indices such as NSE, KGE, pbias, etc. for discharge
gof_flow <- calc_gof(sim = flow_sim, obs = flow_obs,
                     funs = list(nse_q = NSE, kge_q = KGE, pb_q = pbias,
                                 mae_q = mae))

# Calculate RSR for different discharge FDC sections (as proposed in
# Pfannerstill et al., 2014 (https://doi.org/10.1016/j.jhydrol.2013.12.044))
gof_fdc <- calc_fdc_rsr(fdc_sim = flow_fdc_sim, fdc_obs = flow_fdc_obs,
                        quantile_splits = c(5, 20, 70, 95))

# Calculate goodness-of-fit for Nitrogen
gof_ntot <- calc_gof(sim = ntot_sim, obs = ntot_obs,
                     funs = list(nse_n = NSE, kge_n = KGE, pb_n = pbias,
                                 mae_n = mae))

# E.g. if all calculated GOF tables should be joined to one table
gof_all <- list(gof_flow, gof_fdc, gof_ntot) %>%
  reduce(., left_join, by = 'run')

# Visual analysis of goodness-of-fit --------------------------------------
# A very useful tool to assess multiple criteria at the same time is to
# plot a parameter identifiability plot.
# I recommend to look at goodness-of-fit indices of different variables
# at the same time to identify trade offs in parameter ranges. Too many
# indices in one plot may be hard to assess. Therefore, a selection of
# indices for plotting is recommended.
gof_sel <- select(gof_all, run, nse_q, pb_q, p_0_5, p_20_70, p_70_95, nse_n, pb_n)

# It is always good to get a general overview of the summary statistics of the
# calculated GOF indices.
summary(gof_sel)

plot_parameter_identifiability(parameters = par_vals,
                               objectives = gof_sel,
                               run_fraction = .2)

# Another typical approach to analyze goodness-of-fit to parameter changes is
# to plot dotty plots. Here is just one example, to get a full picture of
# how to update parameter ranges the assessment of multiple dotty plots may
# be necessary.
plot_dotty(par = parameter, gof_all$nse_q, n_col = 5)

# If the model performance must be improved and e.g. the parameter
# identifiability plot clearly suggests to update parameter ranges for relevant
# parameters it is recommended to go back to step 1, update the parameter
# boundaries, perform simulations for the new parameter combinations and again
# analyze them with this script.

# Selection of parameter sets ---------------------------------------------
# If the model performance is acceptable there are different approaches to select
# acceptable parameter sets, e.g. by defining thresholds for goodness-of-fit
# indices, or by ranking model runs. The following gives 3 examples to
# identify an ensemble of acceptable parameter combinations.

# Ranking simulations runs based on ranks or normalized values of
# goodness-of-fit indices. Careful here that ranking is always done from smallest
# to largest value. Therefore the indices must be modified before.
gof_adj <- gof_sel %>%
  mutate(nse_q = - nse_q,
         pb_q = - abs(pb_q),
         nse_n = - nse_n,
         pb_n = - abs(pb_n))

# Ranking could be done with two different types of ranking either type = 'rank'
# which uses absolute ranks, or with type = 'norm' which uses normalized
# goodness-of-fit values and therefore accounts for differences in the
# performances.
gof_rank <- rank_gof(gof_adj, type = 'norm')
# Here just the first 10 runs of the ranked GOF indices are selected.
run_sel <- gof_rank$run[1:10]
run_ids <- run_to_id(run_sel)

# The second approach is to select parameter combinations based on threshold
# values for GOF indices. Here e.g. for NSE and pbias for discharge and N loads.
run_sel <- which(gof_sel$nse_q > 0.5 & gof_sel$nse_n > 0.7 &
                 abs(gof_sel$pb_q) < 5 & abs(gof_sel$pb_n) < 20)
run_ids <- run_to_id(run_sel)

# View simulated time series ----------------------------------------------
# The simulated time series should be plotted to analyze the strengths and
# weaknesses of the model simulations. SWATtunR offers some interactive
# plot view functions.
view_timeseries(flow_sim, flow_obs, run_ids = run_ids)
