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
#' # Example usage:
#' # plot_dotty(my_data, 'performance', y_label = 'y_label')
#'
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
