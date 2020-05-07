library('virsim')
library('ggplot2')
library('gridExtra')
library('data.table')


plot_scen <- function(x, y,
                      ncol = 1,
                      IC_adm_data = NULL,
                      time_breaks = 200,
                      max_cases = 6e3,
                      case_breaks = 2e3,
                      max_IC = 1900 / 17.4,
                      IC_case_breaks = 20,
                      immunity_target = 60,
                      IC_prob = 1 / 264,
                      y_include = 1,
                      plot_IC_data = TRUE,
                      time_range = NULL,
                      intervention_linetype = 3,
                      theme_choice = theme_classic(),
                      scale_margin = 6) {
  
  x <- copy(x)
  y <- copy(y)
  
  N_pop <- x[time == min(time)][seed == min(seed), sum(S + E + I + R)]
  x <- x[, lapply(.SD, function(x) x / N_pop * 1e6),
         .SDcols = c("S", "E", "I", "R", "inc", "IC_prev"),
         by = .(seed, time)]
  
  y_include <- max(y_include, x[, I])
  
  intervention_times <- y[, intervention_t[[1]]]
  
  x_axis_definition <- scale_x_continuous(name = "\nTime since start of strategy (days)",
                                          breaks = time_breaks * -1e3:1e3)
  
  theme_choice <- theme_choice + theme(axis.title = element_blank())
  
  panel_A <-
    ggplot(data = x,
           mapping = aes(x = time, y = I, group = seed)) +
    theme_choice
  
  if (any(!is.na(intervention_times))) {
    panel_A <- panel_A +
      geom_vline(xintercept = intervention_times,
                 col = "darkgrey", linetype = intervention_linetype)
  }
  
  panel_A <- panel_A +
    geom_hline(yintercept = max_cases, col = "red", linetype = 2) +
    geom_line(size = .2) +
    x_axis_definition + 
    scale_y_continuous(name = NULL,
                       # name = "Number of infectious cases\nper million\n",
                       breaks = case_breaks * 0:1e3,
                       labels = scales::comma) +
    # labs(tag = "A") +
    expand_limits(y = c(0, y_include)) +
    labs(title = "Number of infectious cases per million")
  
  if (!is.null(time_range)) {
    panel_A <- panel_A + coord_cartesian(x = time_range)
  }
  
  panel_B <-
    ggplot(data = x,
           mapping = aes(x = time, y = IC_prev, group = seed)) +
    theme_choice
  
  if (any(!is.na(intervention_times))) {
    panel_B <- panel_B +
      geom_vline(xintercept = intervention_times,
                 col = "darkgrey", linetype = intervention_linetype)
  }
  
  panel_B <- panel_B +
    geom_hline(yintercept = max_IC,
               col = "red", linetype = 2) +
    geom_line(size = .2) +
    x_axis_definition + 
    scale_y_continuous(name = NULL,
                       # name = "Number of cases in IC\nper million\n",
                       breaks = IC_case_breaks * 0:1e3,
                       labels = scales::comma) +
    # labs(tag = "B") +
    coord_cartesian(x = time_range) +
    labs(title = "Number of cases in IC per million")
  
  if (plot_IC_data) {
    panel_B <- panel_B +
      geom_point(data = IC_adm_data,
                 mapping = aes(x = Day_since_lockdown + intervention_times[2],
                               y = NICE / 17.4,
                               group = NULL),
                 size = 0.8, col = "red", alpha = .75, shape = 1)
  }
  if (!is.null(time_range)) {
    panel_B <- panel_B + coord_cartesian(x = time_range)
  }
  
  panel_C <-
    ggplot(data = x,
           mapping = aes(x = time,
                         y = 100 * R / (S + E + I + R),
                         group = seed)) +
    theme_choice
  
  if (any(!is.na(intervention_times))) {
    panel_C <- panel_C +
      geom_vline(xintercept = intervention_times,
                 col = "darkgrey", linetype = intervention_linetype)
  }
  
  panel_C <- panel_C +
    geom_hline(yintercept = immunity_target,
               col = "darkgrey", linetype = 2) +
    geom_line(size = .2) +
    x_axis_definition + 
    scale_y_continuous(name = NULL,
                       # name = "Percentage\nrecovered (%)\n",
                       breaks = 20 * 0:5) +
    # labs(tag = "C") +
    labs(title = "Percentage recovered (%)") +
    expand_limits(y = c(0, 100))
  
  if (!is.null(time_range)) {
    panel_C <- panel_C + coord_cartesian(x = time_range)
  }
  
  if (ncol == 1) {
    margin_A <- theme(plot.margin = unit(rep(6, 4) + scale_margin * c(1, rep(0, 3)), "pt"))
    margin_B <- theme(plot.margin = unit(rep(6, 4) + scale_margin * c(.5, 0, .5, 0), "pt"))
    margin_C <- theme(plot.margin = unit(rep(6, 4) + scale_margin * c(0, 0, 1, 0), "pt"))
  } else {
    margin_A <- theme(plot.margin = unit(rep(6, 4) + scale_margin * rep(1, 4), "pt"))
    margin_B <- theme(plot.margin = unit(rep(6, 4) + scale_margin * rep(1, 4), "pt"))
    margin_C <- theme(plot.margin = unit(rep(6, 4) + scale_margin * rep(1, 4), "pt"))
  }
  
  gA <- ggplotGrob(panel_A + margin_A)
  gB <- ggplotGrob(panel_B + margin_B)
  gC <- ggplotGrob(panel_C + margin_C)
  
  maxWidth = grid::unit.pmax(gA$widths[2:5],
                             gB$widths[2:5],
                             gC$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  gC$widths[2:5] <- as.list(maxWidth)
  grid.arrange(gA, gB, gC,
               ncol = ncol,
               bottom = "Time since start of strategy (days)")
  
}
