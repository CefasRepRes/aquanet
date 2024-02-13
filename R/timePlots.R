#' timePlots
#'
#' Time plots created displaying cumulative number of infected sites on the y axis and time in days on the x axis.
#' @param scenario_name (class string) the name of the scenario being loaded.
#' @param y_max maximum number of infected sites
#'
#' @return outputPlot
#' @import here
#' @import ggplot2
#' @import data.table
#' @importFrom stats smooth.spline
#' @export
#'

timePlot <- function(scenario_name,
                     y_max){
  # define column names used with dplyr syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  t_simplified <- cumulative_no_infected_sites <- sim_no <- x <- y <- . <- NULL
  # Load results
  scenario <- data.table(aquanet::loadResultsSummary(scenario_name))
  class(scenario)

  # Round time to the nearest day
  scenario$t_simplified <- round(scenario$t, digits = 0)

  # Average across days
  scenario_average <- scenario[, .(mean = mean(cumulative_no_infected_sites)), by = t_simplified]
  nrow(scenario_average)
  scenario_count <- scenario[, .N, by = t_simplified,]
  scenario_summary <- merge(scenario_average,scenario_count, by = 't_simplified', nomatch = 0)

  # Smooth the average weighted by count
  scenario_smooth <- smooth.spline(x = scenario_summary$t_simplified,
                                   y = scenario_summary$mean,
                                   w = scenario_summary$n)
  scenario_smooth <- data.frame(x = scenario_smooth$x,
                                y = scenario_smooth$y,
                                sim_no = NA)
  # Plot trajectories
  outputPlot <- ggplot(scenario, aes(x = t, y = cumulative_no_infected_sites, group = sim_no)) +
    geom_line(col = "#0072B2", alpha = 0.08) +
    ylim(0, y_max) +
    xlab("Time (days)") +
    ylab("Infected sites \n(cumulative)") +
    geom_line(data = scenario_smooth, aes(x = x, y = y)) +
    theme_light()

  #create plot folder to save plot if does not exist
  if (!file.exists(here::here("outputs","plots"))){
    dir.create(here::here("outputs","plots"))
    cat("Directory 'plots' created\n")
  } else {
    cat("Directory 'plots' already exists\n")
  }

  #save as png
  ggsave(here("outputs", "plots", paste(scenario_name, ".png", sep = "")), plot= outputPlot)
  return(outputPlot)
}

