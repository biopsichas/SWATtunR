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
#' @importFrom ggplot2 ggplot geom_boxplot geom_hline facet_grid scale_x_discrete scale_fill_manual labs theme_bw theme axis.text.x axis.title.x
#' @importFrom dplyr filter select pivot_longer arrange enframe mutate left_join
#' @export

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
               linetype = "solid", linewidth = 0.75) +
    geom_hline(data = yld_obs, aes(yintercept = yield_min), color = "tomato1",
               linetype = "solid") +
    geom_hline(data = yld_obs, aes(yintercept = yield_max), color = "tomato1",
               linetype = "solid") +
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
  gg_phu / gg_yld / gg_bms
}
