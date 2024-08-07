#' Plot Calibration-Validation Comparison
#'
#' This function takes calibration and validation tables and produces a
#' comparison boxplot for various metrics.
#'
#' @param cal_tbl A data frame containing calibration performance results.
#' @param val_tbl A data frame containing validation performance results.
#' @param indexes (optional) An optional vector of metric names to filter and
#' include in the comparison plot. Default \code{indexes = NULL} includes all
#' metrics.
#'
#' @return A ggplot object representing the comparison between calibration and
#' validation results for the specified metrics.
#' @importFrom dplyr bind_rows mutate filter select ends_with
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_boxplot facet_wrap geom_jitter xlab guides theme_bw theme element_blank  guide_legend
#'
#' @export
#' @examples
#' \dontrun{
#' plot_calval_comparison(obj_tbl_cal, obj_tbl_val, indexes = c("nse", "pbias"))
#' }
#' @keywords plot
#' @seealso \code{\link{calculate_performance}}, \code{\link{calculate_performance_2plus}}
#'

plot_calval_comparison <- function(cal_tbl, val_tbl, indexes = NULL){
  # Combine the results from calibration and validation period
  comb_results <- bind_rows(cal_tbl %>% mutate(type = "calibration"),
                            val_tbl %>% mutate(type = "validation")) %>%
    select(-c(run_id, ends_with("tot"))) %>%
    pivot_longer(-type, names_to = "metric", values_to = "value")
  # Filter the results if only some metrics should be compared
  if(!is.null(indexes)){
    comb_results <- filter(comb_results, metric %in% indexes)
  }

  # Compare in the figure
  fig <- ggplot(comb_results, aes(x = type, y = value, fill = type)) +
    geom_boxplot() +
    facet_wrap(~metric, scales = "free_y") +
    geom_jitter(color="black", alpha=0.4) +
    xlab('') +
    guides(fill = guide_legend(title = NULL)) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  return(fig)
}


#' Function to plot OAT (One-At-A-Time) analysis results
#'
#' This function generates an interactive dygraph to visualize the results of an OAT analysis,
#' comparing simulated data with observed data if provided.
#'
#' @param sim Object containing simulation data from SWATrunR.
#' @param obs Optional dataframe for observed data with columns 'date' and 'value'. Default is NULL.
#' @param variable Name of the variable for which OAT analysis results are plotted.
#' @param round_values Number of decimal places to round parameter values. Default is 3.
#'
#' @return A dygraph object showing the OAT analysis results.
#'
#' @importFrom xts xts
#' @importFrom dplyr select left_join
#' @importFrom dygraphs dygraph dyOptions dySeries dyRangeSelector dyHighlight dyCSS
#' @importFrom stringr str_replace_all
#' @importFrom purrr map set_names
#' @importFrom tibble as_tibble
#' @importFrom readr parse_number
#' @export
#'
#' @examples
#' \dontrun{
#' plot_oat(sim_oat, obs = obs_data, variable = 'flo_day', round_values = 2)
#' }
#' @seealso \code{\link{plot_selected_sim}}, \code{\link{sample_oat}}, \code{\link[SWATrunR:run_swatplus]{https://chrisschuerz.github.io/SWATrunR/}}


plot_oat <- function(sim, obs = NULL, variable, round_values = 3) {
  if(!is.null(obs)) {
    if(!is.Date(obs[[1]])){
      stop("The first column of 'obs' must by of type 'Date'.")
    }
    names(obs) <- c('date', 'obs')
  }

  par_oat <- sim$parameter$values

  parameter <- names(par_oat)[map(names(par_oat), ~length(unique(par_oat[[.]]))) > 1]

  run_name <- paste0('run_',
                     sprintf(paste0('%0',
                                    nchar(as.character(nrow(par_oat))), 'd'),
                             as.numeric(row.names(par_oat))))

  run_exist <- run_name %in% names(sim$simulation[[variable]])
  sim_sel <- sim$simulation[[variable]][, c('date', run_name[run_exist])] %>%
    set_names(c('date', paste0(parameter,':', round(par_oat[as.vector(parse_number(run_name[run_exist])), parameter][[1]], round_values))))

  col_pal <- c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
               "#A65628", "#F781BF", "#999999", "#1B9E77", "#D9D9D9")
  col_pal <- col_pal[run_exist]

  if (!is.null(obs)) {
    sim_sel <- left_join(sim_sel, obs, by = 'date')
    col_pal <- c(col_pal, 'black')
  }

  sim_sel_xts <- xts(select(sim_sel, -date), order.by = sim_sel$date)



  dy_oat <- dygraph(sim_sel_xts) %>%
    dyRangeSelector(height = 100, fillColor = 'gray', strokeColor = 'black') %>%
    dyOptions(colors = col_pal) %>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.6,
                hideOnMouseOut = FALSE) %>%
    dyCSS(textConnection("
     .dygraph-legend > span.highlight { display: inline; background-color: #B0B0B0;}
  "))

  if (!is.null(obs)) {
    dy_oat <- dySeries(dy_oat, 'obs', drawPoints = TRUE,
                       pointSize = 2, strokeWidth = 0.75)
  }

  return(dy_oat)
}

#' Plot interactive time series of simulation results
#'
#' This function plots interactive time series comparing simulated data with observed data.
#'
#' @param sim Object from SWATrunR.
#' @param obs (optional) Dataframe for observed data with columns 'date' and
#' 'value'. Default \code{obs = NULL}, only simulation data are available.
#' @param par_name (optional) Name of the parameter set to use.
#' Default \code{par_name = NULL} uses the first variable in sim object.
#' For multiple parameter sets, specify the one to use, e.g.,
#' par_name = "flo_day" or par_name = "no3_day_conc".
#' @param run_ids (optional) Character vector of run IDs to plot.
#' Default \code{run_ids = NULL} requires input of run_sel.
#' @param run_sel (optional) Character vector of run IDs to emphasize in the plot.
#'  Default \code{run_sel = NULL} requires input of run_ids.
#' @param plot_bands (optional) Logical. If TRUE, includes lower and upper bands of results.
#' Default \code{plot_bands = TRUE}.
#' @param period (optional) Character defining the time step for aggregation. Options include "day", "week",
#' "month", "year", etc. Use "average monthly" for multi-annual monthly values.
#' Default \code{period = NULL} provides no aggregation. See [lubridate::floor_date](https://www.rdocumentation.org/packages/lubridate/versions/1.3.3/topics/floor_date) for details.
#' @param fn_summarize Function to summarize time intervals. Default \code{fn_summarize ="mean"}, other examples are "median", "sum".
#' See [dplyr::summarise](https://dplyr.tidyverse.org/reference/summarise.html) for details.
#' @param x_label (optional) Character for x axis label. Default \code{x_label = "Date"}.
#' @param y_label (optional) Character for y axis label. Default \code{y_label = "Discharge (m³/s)"}.
#'
#' @return Interactive plot of simulated and observed data.
#' @export
#'
#' @importFrom dplyr mutate group_by summarize_all select left_join bind_cols
#' @importFrom tibble add_column
#' @importFrom xts xts
#' @importFrom dygraphs dygraph dyRangeSelector dySeries dyHighlight
#' @importFrom lubridate is.Date month floor_date
#' @examples
#' \dontrun{
#' plot_selected_sim(sim_flow, obs = obs, run_sel = c(95), run_ids = run_sel_ids,
#' period = "average monthly", plot_bands = TRUE)
#' }
#' @keywords plot

plot_selected_sim <- function(sim, obs = NULL, par_name = NULL, run_ids = NULL, run_sel = NULL, plot_bands = TRUE,
                              period = NULL, fn_summarize = 'mean',
                              x_label = 'Date', y_label = "Discharge (m<sup>3</sup> s<sup>-1</sup>)") {

  if(is.null(par_name)) {
    # Filter to parameter set of interest.
    sim <- sim$simulation[[1]]
  } else {
    sim <- sim$simulation[[par_name]]
  }

  # Change the time step to the period of interest.
  if(!is.null(period)) {
    if(period == "average monthly"){
      sim <- mutate(sim, date = month(date)) %>%
        group_by(date) %>%
        summarize_all(get(fn_summarize))
      obs <- mutate(obs, date = month(date)) %>%
        group_by(date) %>%
        summarize_all(get(fn_summarize))
    } else {
      sim <- mutate(sim, date = floor_date(date , period)) %>%
        group_by(date) %>%
        summarize_all(get(fn_summarize))
      obs <- mutate(obs, date = floor_date(date , period)) %>%
        group_by(date) %>%
        summarize_all(get(fn_summarize))
    }
  }


  if(is.null(run_ids) & is.null(run_sel)) {
    stop("At least one of 'run_ids' or 'run_sel' must be provided.")
  }
  if(!is.Date(sim[[1]])){
    if(!is.null(period) && period != "average monthly"){
      stop("The first column of 'sim' must by of type 'Date'.")
    }
  }

  dy_tbl <- select(sim, date)

  if(!is.null(obs)) {
    if(!is.Date(obs[[1]])){
      if(!is.null(period) && period != "average monthly"){
        stop("The first column of 'obs' must by of type 'Date'.")
      }
    }
    names(obs) <- c('date', 'obs')
    dy_tbl <- left_join(dy_tbl, obs, by = 'date')
  }

  nchar_run <- nchar(names(sim)[2]) - 4

  if(!is.null(run_sel)) {
    run_sel <- paste0('run_', sprintf(paste0('%0', nchar_run, 'd'), as.numeric(run_sel)))
    dy_tbl <- add_column(dy_tbl, sim_sel = sim[, run_sel])
  }

  if(!is.null(run_ids)) {
    run_ids <- paste0('run_', sprintf(paste0('%0', nchar_run, 'd'), as.numeric(run_ids)))
    sim_ids <- sim[, run_ids]
    if(plot_bands) {
      sim_upr <- apply(sim_ids, 1, max)
      sim_lwr <- apply(sim_ids, 1, min)
      dy_tbl <- add_column(dy_tbl, upr   = sim_upr, lwr   = sim_lwr,
                           upr_l = sim_upr, lwr_l = sim_lwr)
    } else {
      dy_tbl <- bind_cols(dy_tbl, sim_ids)
    }
  }

  if (is.null(period) || period != "average monthly") {
    dy_xts <- xts(dy_tbl[,-1], order.by = dy_tbl[,1][[1]])
  } else {
    dy_xts <- xts(dy_tbl[,-1], order.by = seq(as.Date("00-01-01"), by = "month", length.out = 12))
  }

  dy_plt <- dy_xts %>%
    dygraph(., xlab = x_label, ylab = y_label) %>%
    dyRangeSelector(height = 30)

  if(!is.null(obs)) {
    dy_plt <- dy_plt %>%
      dySeries('obs', color = 'black', drawPoints = TRUE,
               pointSize = 2, strokeWidth = 0.75)
  }

  if(plot_bands) {
    if(!is.null(run_ids) & !is.null(run_sel)) {
      dy_plt <- dy_plt %>%
        dySeries(c("lwr", "sim_sel", "upr"), label = "sim_sel",
                 color =  "#A50F15", strokeWidth = 1.2,
                 drawPoints = TRUE, pointSize = 2) %>%
        dySeries("lwr_l", label = "lower", color =  "#CB181D",
                 strokePattern = 'dashed') %>%
        dySeries("upr_l", label = "upper", color =  "#CB181D",
                 strokePattern = 'dashed')
    } else if (is.null(run_ids)) {
      dy_plt <- dy_plt %>%
        dySeries("sim_sel", label = "sim_sel",
                 color =  "#A50F15", strokeWidth = 1.2,
                 drawPoints = TRUE, pointSize = 2)
    } else {
      dy_plt <- dy_plt %>%
        dySeries(c("lwr", "upr_l", "upr"), label = "upper",
                 color =  "#CB181D", strokePattern = 'dashed') %>%
        dySeries("lwr_l", label = "lower", color =  "#CB181D",
                 strokePattern = 'dashed')
    }
  } else {
    col_pal <- c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
                 "#A65628", "#F781BF", "#999999", "#1B9E77", "#D9D9D9")
    if(!is.null(run_ids)) {
      for(i in 1:length(run_ids)) {
        dy_plt <- dy_plt %>%
          dySeries(run_ids[i], label = run_ids[i],
                   color =  col_pal[i], strokeWidth = 1)
      }
      dy_plt <- dy_plt %>%
        dyHighlight(highlightSeriesBackgroundAlpha = 0.6,
                    hideOnMouseOut = TRUE)
    }
    if (!is.null(run_sel)) {
      dy_plt <- dy_plt %>%
        dySeries("sim_sel", label = "sim_sel",
                 color =  "#A50F15", strokeWidth = 1.2,
                 drawPoints = TRUE, pointSize = 2)
    }
  }

  return(dy_plt)
}


#' Plot Parameter Identifiability
#'
#' This function creates a plot to visualize the identifiability of parameters
#' based on objective functions.
#'
#' @param parameters A vector of parameter names.
#' @param objectives A list of objective functions, each represented as a numeric vector.
#' @param run_fraction (optional) A numeric value representing the fraction of
#' runs to consider for threshold calculation. Default \code{run_fraction = NULL},
#' which defines this parameter according method provided in the function.
#'
#' @return A ggplot object visualizing the identifiability of parameters.
#'
#' @importFrom ggplot2 scale_y_continuous scale_fill_gradientn geom_rect facet_wrap labs theme_bw theme unit element_blank
#' @importFrom dplyr mutate %>%
#' @importFrom purrr map_dbl map2_df
#' @importFrom scales rescale
#' @importFrom tibble tibble
#'
#' @export


plot_parameter_identifiability <- function(parameters, objectives, run_fraction = NULL) {
  if(is.null(run_fraction)) {
    nb_run <- dim(parameters)[1]
    if(nb_run >= 5000){
      run_fraction <- 0.15 - ((nb_run - 5000)/100000)
    } else if(nb_run >= 1000){
      run_fraction <- 0.225 - ((nb_run - 1000)/40000)
    } else if (nb_run >= 100){
      run_fraction <- 0.5 - ((nb_run - 100)/3000)
    } else {
      run_fraction <- 0.5
    }
    message(paste0("The number of runs is ", nb_run, ". The 'run_fraction' parameter is set to ", run_fraction))
  }
  thresholds <- map_dbl(objectives, ~ quantile(.x, 1 - run_fraction))
  plot_tbl <- map2(objectives, thresholds,
                   ~ calc_segment_diff(parameters, .x, .y)) %>%
    map2_df(., 1:length(.), ~ mutate(.x, id = .y))

  lbls <- tibble(obj = names(objectives), y = 1:ncol(objectives))

  col_vals <- c(seq(-100, -25, length.out = 4), seq(0, max(100, max(plot_tbl$fill_segment)), length.out = 5))
  col_pal <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7",
               "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
  # col_pal <- c(col_pal_full[1], col_pal_full[(6 - n_neg):4], col_pal_full[5:9])
  ggplot(plot_tbl) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = id - 0.5, ymax = id + 0.5, fill = fill_segment)) +
    facet_wrap(.~parameter, scales = 'free_x') +
    scale_y_continuous(breaks = lbls$y, labels = lbls$obj) +
    scale_fill_gradientn(colors = col_pal, values = rescale(col_vals),
                         limits = c(-100, max(100, max(plot_tbl$fill_segment)))) +
    labs(fill = 'Deviation to uniform distribution (%)') +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.key.width = unit(2, 'cm'),
          panel.grid.minor.y = element_blank())
}


#' Plot PHU, yield, and biomass over days to maturity
#'
#' This function generates boxplots to visualize Plant Heat Units (PHU) fractions,
#' yields, and biomass over changes in days to maturity.
#'
#' @param sim_result The simulation results containing PHU, yield, and biomass data.
#' @param x_label Labels for the x-axis representing changes in days to maturity.
#' @param yield (optional) The mean yield values for each crop.
#' Default \code{yield = NULL}.
#' @param yield_min (optional) The minimum observed yield values for each crop.
#' Default \code{yield_min = NULL}
#' @param yield_max (optional) The maximum observed yield values for each crop.
#' Default \code{yield_max = NULL}
#' @return A combined ggplot object showing boxplots for PHU fractions, yields, and biomass.
#' @importFrom ggplot2 ggplot geom_boxplot geom_hline facet_grid scale_x_discrete
#' scale_fill_manual coord_cartesian labs theme_bw theme element_blank aes
#' element_text vars
#' @importFrom dplyr filter select arrange mutate left_join
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom tibble enframe
#' @importFrom lubridate year
#' @importFrom gridExtra grid.arrange
#' @export
#' @examples
#' \dontrun{
#' plot_phu_yld_bms(ylds_phu_dmat, dmat_chg, yield_mean, yield_min, yield_max)
#' }
#' @keywords plot

plot_phu_yld_bms <- function(sim_result, x_label, yield = NULL,
                             yield_min = NULL, yield_max = NULL) {
  # mgtout also prints the skipped years, therefore limit to the years after the
  # skipped time period
  start_year <- year(sim_result$run_info$simulation_period$start_date)
  end_year   <- year(sim_result$run_info$simulation_period$end_date)
  if(!is.na(sim_result$run_info$simulation_period$years_skip)) {
    start_year <- year(sim_result$run_info$simulation_period$start_date)
    start_year <- start_year + sim_result$run_info$simulation_period$years_skip
  } else {
    start_year <- year(sim_result$run_info$simulation_period$start_date_print)
  }

  sim_years <- start_year:end_year

  # Prepare the simulations of PHU fractions for plotting
  phu <- sim_result$simulation$phu %>%
    filter(year %in% sim_years) %>%
    select(-year, -hru) %>%
    pivot_longer(., cols = - plant_name) %>%
    arrange(plant_name)

  col_pal <- c("#7570B3", "#8DD3C7", "#666666", "#FFFFB3", "#D95F02", "#66A61E",
               "#BEBADA", "#FB8072", "#80B1D3", "#E7298A", "#FDB462", "#B3DE69",
               "#A6761D", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F",
               "#1B9E77", "#E6AB02")
  #Plot PHU fractions per crop over the days_mat changes
  gg_phu <- ggplot(data = phu) +
    geom_boxplot(aes(x = name, y = value, fill = name)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_hline(yintercept = 1.25, linetype = 'dashed') +
    facet_grid(cols = vars(plant_name)) +
    scale_x_discrete(labels = x_label) +
    scale_fill_manual(values = col_pal) +
    coord_cartesian(ylim = c(0, 2)) +
    labs(y = 'PHU fraction at harves/kill') +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = 'none')

  # Prepare the simulations of yields for plotting
  yld <- sim_result$simulation$yld %>%
    filter(year %in% sim_years) %>%
    select(-year, -hru) %>%
    pivot_longer(., cols = - plant_name) %>%
    mutate(value = value/1000) %>%
    arrange(plant_name)

  yld_mean <- enframe(yield, name = 'plant_name', value = 'yield_mean') %>%
    mutate(plant_name = as.character(plant_name))
  yld_min  <- enframe(yield_min, name = 'plant_name', value = 'yield_min') %>%
    mutate(plant_name = as.character(plant_name))
  yld_max  <- enframe(yield_max, name = 'plant_name', value = 'yield_max') %>%
    mutate(plant_name = as.character(plant_name))

  yld_obs <- left_join(yld_mean, yld_min, by = 'plant_name') %>%
    left_join(., yld_max, by = 'plant_name') %>%
    filter(plant_name %in% yld$plant_name)

  #Plot yields per crop over the days_mat changes
  gg_yld <- ggplot(data = yld) +
    geom_boxplot(aes(x = name, y = value, fill = name)) +
    facet_grid(cols = vars(plant_name)) +
    scale_x_discrete(labels = x_label) +
    scale_fill_manual(values = col_pal) +
    labs(y = 'Yield (t/ha)', x = 'days_mat absval change') +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          legend.position = 'none') +
    geom_hline(data = yld_obs, aes(yintercept = yield_mean), color = "tomato3",
               linetype = "solid", linewidth = 1) +
    geom_hline(data = yld_obs, aes(yintercept = yield_min), color = "tomato1",
               linetype = "dashed") +
    geom_hline(data = yld_obs, aes(yintercept = yield_max), color = "tomato1",
               linetype = "dashed") +
    labs(caption = "Red lines represent observed values.")

  # Prepare the simulations of yields for plotting
  bms <- sim_result$simulation$bms %>%
    filter(year %in% sim_years) %>%
    select(-year, -hru) %>%
    pivot_longer(., cols = - plant_name) %>%
    mutate(value = value/1000) %>%
    arrange(plant_name)

  #Plot yields per crop over the days_mat changes
  gg_bms <- ggplot(data = bms) +
    geom_boxplot(aes(x = name, y = value, fill = name)) +
    facet_grid(cols = vars(plant_name)) +
    facet_grid(cols = vars(plant_name)) +
    scale_x_discrete(labels = x_label) +
    scale_fill_manual(values = col_pal) +
    labs(y = 'Bio mass (t/ha)', x = 'days_mat absval change') +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = 'none')

  # Put the PHU plot and the yield plot together
  grid.arrange(gg_phu, gg_yld, gg_bms, ncol = 1)
}

#' Plot Dotty Yields
#'
#' This function creates dotty plots for simulated yields, comparing them with
#' observed yields (if available).
#'
#' @param sim_result The simulation results containing yield data.
#' @param yield (optional) The mean yield values for each crop.
#' Default \code{yield = NULL}.
#' @param yield_min (optional) The minimum observed yield values for each crop.
#' Default \code{yield_min = NULL}
#' @param yield_max (optional) The maximum observed yield values for each crop.
#' Default \code{yield_max = NULL}
#' @return A combined ggplot object showing dottty plots for 4 parameters.
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove
#' @importFrom ggplot2 labs
#' @importFrom gridExtra grid.arrange
#' @importFrom purrr map
#' @importFrom dplyr %>% mutate arrange left_join rename any_of
#' @importFrom lubridate year
#' @export
#' @examples
#' \dontrun{
#' plot_phu_yld_bms(ylds_phu_dmat, dmat_chg, yield_mean, yield_min, yield_max)
#' }
#' @keywords plot

plot_dotty_yields <- function(sim_result, yield = NULL, yield_min = NULL, yield_max = NULL) {
  ##Years to filter
  start_year <- year(sim_result$run_info$simulation_period$start_date)+
    sim_result$run_info$simulation_period$years_skip
  end_year <- year(sim_result$run_info$simulation_period$end_date)

  par <- sim_result$parameter$values %>%
    mutate(run = 1:nrow(.))
  # Prepare yields from simulations in data frame for plotting
  yld <- sim_result$simulation$yld %>%
    filter(year %in% start_year:end_year) %>%
    select(-year, -hru) %>%
    pivot_longer(., cols =  starts_with('run'), names_to = 'run') %>%
    mutate(value = value/1000,
           run = as.numeric(str_remove(run, 'run_'))) %>%
    arrange(plant_name) %>%
    left_join(., par, by = c('run')) %>%
    select(-any_of(c("run", "date", "name"))) %>%
    pivot_longer(!c(plant_name, value), names_to = "parameter",
                 values_to = "values") %>%
    rename(yield = value)

  plt_list <- map(unique(yld$parameter),
                  ~ dotty_fig(yld[yld$parameter == .x,], yield, yield_min, yield_max))

  plt_list[[length(plt_list)]] <- plt_list[[length(plt_list)]] +
    labs(caption = "Red lines represent observed values")
  grid.arrange(grobs = plt_list, ncol = 1,
               left = "Yields (t/ha)", bottom = "Value change")
}

#' Make a dotty plot
#'
#' @param sim_yield a dataframe with yield data
#' @param yield (optional) The mean yield values for each crop.
#' Default \code{yield = NULL}.
#' @param yield_min (optional) The minimum observed yield values for each crop.
#' Default \code{yield_min = NULL}.
#' @param yield_max (optional) The maximum observed yield values for each crop.
#' Default \code{yield_max = NULL}.
#' @importFrom tibble enframe
#' @importFrom dplyr mutate left_join filter group_by summarise
#' @importFrom ggplot2 ggplot geom_pointrange geom_smooth theme element_text element_blank geom_hline facet_grid theme_bw
#' @importFrom stats quantile
#' @return ggplot object for dotty plot
#' @keywords internal
#' @examples
#' \dontrun{
#' dotty_fig(sim_yield)
#' }
#' @keywords plot


dotty_fig <- function(sim_yield, yield = NULL, yield_min = NULL, yield_max = NULL){
  yld_mean <- enframe(yield, name = 'plant_name', value = 'yield_mean') %>%
    mutate(plant_name = as.character(plant_name))
  yld_min  <- enframe(yield_min, name = 'plant_name', value = 'yield_min') %>%
    mutate(plant_name = as.character(plant_name))
  yld_max  <- enframe(yield_max, name = 'plant_name', value = 'yield_max') %>%
    mutate(plant_name = as.character(plant_name))

  yld_obs <- left_join(yld_mean, yld_min, by = 'plant_name') %>%
    left_join(., yld_max, by = 'plant_name') %>%
    filter(plant_name %in% sim_yield$plant_name)

  df_rng <- sim_yield %>%
    group_by(plant_name, parameter, values) %>%
    summarise(yld_50  = quantile(yield, probs = 0.5),
              yld_25 = quantile(yield, probs = 0.25),
              yld_75 = quantile(yield, probs = 0.75), .groups = 'drop')

  ggplot()+
    geom_pointrange(data = df_rng, aes(x = values, y = yld_50,
                                       ymin = yld_25, ymax = yld_75),
                    shape = 18) +
    geom_smooth(data = df_rng, aes(x = values, y = yld_50),
                method = 'glm', formula = 'y ~ x') +
    geom_hline(data = yld_obs, aes(yintercept = yield_mean), color = "tomato3",
               linetype = "solid", linewidth = 0.75) +
    geom_hline(data = yld_obs, aes(yintercept = yield_min), color = "tomato1",
               linetype = "twodash") +
    geom_hline(data = yld_obs, aes(yintercept = yield_max), color = "tomato1",
               linetype = "twodash") +
    facet_grid(parameter~plant_name, scales = "free")+
    theme_bw()+
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle=30))
}

#' Plot Dotty
#'
#' This function creates a dotty plot to visualize the relationship between
#' parameter values and performance results.
#'
#' @param par A data frame of model parameter values for each simulation.
#' @param var The model performance result vector or list of vectors to be
#' plotted against parameter values.
#' @param y_label (optional) Labels for the y-axis, either a single label or a vector
#' corresponding to each variable if `var` is a list. Default \code{y_label = 'y'}.
#' @param n_col (optional) Number of columns in the facet grid. Default \code{n_col = 3}.
#' @param y_lim (optional) Limits for the y-axis. Default \code{y_lim = NULL}.
#' @param y_inter (optional) Y-axis intercept value. Default \code{y_inter = NULL}.
#' @param trend (optional) Logical, indicating whether to add a trend line.
#' Default is \code{trend = FALSE}.
#' @param run_ids (optional) A numeric vector of run IDs to be highlighted in the plot.
#' Default is \code{run_ids = NULL}.
#' @param low_up (optional) Logical, TRUE if whole possible parameter range should be used
#' for x axis. Default is \code{low_up = FALSE}.
#' @importFrom dplyr %>% select filter left_join rename bind_rows
#' @importFrom ggplot2 ggplot geom_point facet_wrap theme_bw theme element_text
#' scale_color_manual geom_blank geom_smooth geom_hline ylim
#' @importFrom tidyr pivot_longer
#' @importFrom purrr reduce modify2
#' @return A ggplot object representing the dotty plot.
#' @export
#'
#' @examples
#' \dontrun{
#' # plot_dotty(my_data, 'performance', y_label = 'y_label')
#' }
#' @keywords plot

plot_dotty <- function(par, var, y_label = 'y', n_col = 3, y_lim = NULL,
                       y_inter = NULL, trend = FALSE, run_ids = NULL,
                       low_up = FALSE) {
  # Define color palette
  col_pal <- c("grey15", "coral1", "orange4", "slateblue", "deeppink",
               "forestgreen", "gold2", "tomato", "tomato4", "steelblue3", "blue",
               "darkmagenta")
  # Check if 'var' is a list and handle accordingly
  if(is.list(var)){
    if(length(var) != length(y_label)){
      stop('var and y_label should have the same number of inputs')
    }
    # Modify the data based on the list input
    dotty_tbl <- modify2(var, y_label, function(.x, .y){par %>%
        mutate(var = .x) %>%
        pivot_longer(., cols = -var, names_to = 'parameter') %>%
        mutate(y_label = as.factor(.y))}) %>%
      reduce(., bind_rows)
  } else {
    # If 'var' is not a list, handle it normally
    dotty_tbl <- par %>%
      mutate(var = var) %>%
      pivot_longer(., cols = -var, names_to = 'parameter')
  }

  # Create the base ggplot object
  gg <- ggplot(data = dotty_tbl, aes(x = value, y = var, group= y_label, colour = y_label)) +
    geom_point(alpha = ifelse(is.null(trend), 1, 0.3)) +
    scale_color_manual(values = col_pal) +
    facet_wrap(.~parameter, ncol = n_col, scales = "free_x") +
    theme_bw()

  # Add points for specific run_ids if provided
  if(!is.null(run_ids)){
    if(is.numeric(run_ids)){
      if(all(run_ids-floor(run_ids)==0)){
        if(is.list(var)){
          dotty_tbl_ids <- modify2(var, y_label, function(.x, .y){par[run_ids,] %>%
              mutate(var = .x[run_ids]) %>%
              pivot_longer(., cols = -var, names_to = 'parameter') %>%
              mutate(y_label = as.factor(.y))}) %>%
            reduce(., bind_rows)
          gg <- gg + geom_point(data = dotty_tbl_ids, size = 3)
        } else {
          dotty_tbl_ids <- par[run_ids,] %>%
            mutate(var = var[run_ids]) %>%
            pivot_longer(., cols = -var, names_to = 'parameter')
          gg <- gg + geom_point(data = dotty_tbl_ids, color = 'red', size = 2)
        }
      } else {
        stop('run_ids should be integer values!!!')
      }
    } else {
      stop('run_ids should be numeric value or numeric vector of values!!!')
    }
  }

  # Add lower and upper bounds if provided
  if(low_up){
    low_up <- read_tbl(paste0(model_path, '/cal_parms.cal'), n_skip = 2) %>%
      rename(parameter = name) %>%
      filter(parameter %in% dotty_tbl$parameter) %>%
      select(parameter, abs_min, abs_max) %>%
      pivot_longer(., cols = -parameter, names_to = 'par', values_to = 'value') %>%
      select(parameter, value)

    # Add var value
    if(is.list(var)){
      low_up$var <- mean(var[[1]])
      low_up$y_label <- y_label[1]
    } else {
      low_up$var <- mean(var)
    }

    # Add dummy data to the plot
    gg <- gg + geom_blank(data=low_up)
  }

  # Customize the plot based on the number of y labels
  if (length(y_label)>1) {
    gg <- gg + labs(x = 'Change of parameter value', y = "Performance results",  color = "Variables")
  } else {
    gg <- gg +
      theme(legend.position="none")+
      labs(x = 'Change of parameter value', y = y_label)
  }

  # Add trend line if specified
  if(trend){
    gg <- gg + geom_smooth(method = "lm", se = FALSE)
  }

  # Set y-axis limits if provided
  if (!is.null(y_lim)) {
    gg <- gg + ylim(y_lim)
  }

  # Add y-axis intercept line if provided
  if (!is.null(y_inter)) {
    gg <- gg + geom_hline(yintercept = y_inter, lty = 2)
  }

  # Return the ggplot object
  return(gg)
}

#' Plot ESCO Range
#'
#' This function creates a ggplot visualization of the ESCO range data,
#' highlighting specific values and limits with horizontal and vertical lines.
#'
#' @param sim The simulation results water balance data. Including:
#'   \describe{
#'     \item{precip}{Variable out of 'basin_wb_day' output file.}
#'     \item{surq_cha}{Variable out of 'basin_wb_day' output file.}
#'     \item{surq_res}{Variable out of 'basin_wb_day' output file.}
#'     \item{latq_cha}{Variable out of 'basin_wb_day' output file.}
#'     \item{latq_res}{Variable out of 'basin_wb_day' output file.}
#'     \item{qtile}{Variable out of 'basin_wb_day' output file.}
#'     \item{flo}{Variable out of ''basin_aqu_day' output file.}
#'   }
#' @param obs_wy_ratio A numeric value representing the observed water yield ratio.
#' @param rel_wyr_lim A numeric value specifying the relative range for acceptable error.
#' @importFrom ggplot2 ggplot geom_line geom_vline geom_text
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_esco_range(sim_esco, obs_wy_ratio)
#' }

plot_esco_range <- function(sim, obs_wy_ratio, rel_wyr_lim = 0.05) {
  ## Calculate the water yield ratio
  wyr_sim <- calculate_wyr(sim)
  ## Get the esco parameter range and and the parameter value
  esco_rng <- find_par_range(sim$parameter$values$esco, wyr_sim,
                             obs_wy_ratio, rel_wyr_lim)
  ## Create the ggplot object
  ggplot() +
    geom_line(aes(x = esco_rng$x, y = esco_rng$y)) +
    geom_hline(yintercept = obs_wy_ratio, color = 'tomato3') +
    geom_hline(yintercept = (1 + rel_wyr_lim)*obs_wy_ratio,
               color = 'tomato3', linetype = 'dashed') +
    geom_hline(yintercept = (1 - rel_wyr_lim)*obs_wy_ratio,
               color = 'tomato3', linetype = 'dashed') +
    geom_vline(xintercept = esco_rng$par_val, color = 'tomato3') +
    geom_vline(xintercept = esco_rng$par_rng[1], color = 'tomato3', linetype = 'dashed') +
    geom_vline(xintercept = esco_rng$par_rng[2], color = 'tomato3', linetype = 'dashed') +
    geom_text(aes(x = c(esco_rng$par_val, esco_rng$par_rng[1], esco_rng$par_rng[2]),
                  y = rep(obs_wy_ratio, 3),
                  label = round(c(esco_rng$par_val, esco_rng$par_rng[1],
                                  esco_rng$par_rng[2]), 2)),
              vjust = - 0.5, hjust = -0.25) +
    labs(x = 'esco', y = 'Water yield ratio') +
    theme_bw()
}

