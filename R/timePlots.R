#' timePlots
#'
#' Time plots created displaying cumulative number of infected sites on the y axis and time in days on the x axis.
#' @param scenario_name (class string) the name of the scenario being loaded.
#' @param scenario_summary (class string) a file path to the scenario summary produced by cumulativeTimeSummary
#' @param y_max (class numeric) maximum number of infected sites
#'
#' @return outputPlot
#' @import here
#' @import ggplot2
#' @import data.table
#' @importFrom stats smooth.spline
#' @export
#'

timePlot <- function(scenario_name,
                     scenario_summary,
                     y_max){
  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  t_simplified <- cumulative_no_infected_sites <- sim_no <- x <- y <- . <- scenario <- NULL

  # load results
  scenario_summary <- read.csv(scenario_summary) %>% data.table()

  # round time to the nearest day
  scenario_summary$t_simplified <- round(scenario_summary$t, digits = 0)

  # average across days
  scenario_average <- scenario_summary[, .(mean = mean(cumulative_no_infected_sites)), by = t_simplified]
  nrow(scenario_average)
  scenario_count <- scenario_summary[, .N, by = t_simplified,]
  scenario_average <- merge(scenario_average, scenario_count, by = 't_simplified', nomatch = 0)

  # Smooth the average weighted by count
  scenario_smooth <- smooth.spline(x = scenario_average$t_simplified,
                                   y = scenario_average$mean,
                                   w = scenario_average$n)
  scenario_smooth <- data.frame(x = scenario_smooth$x,
                                y = scenario_smooth$y,
                                sim_no = NA)
  # Plot trajectories
  outputPlot <- ggplot(scenario_summary, aes(x = t, y = cumulative_no_infected_sites, group = sim_no)) +
    geom_line(col = "#0072B2", alpha = 0.08) +
    ylim(0, y_max) +
    xlab("Time (days)") +
    ylab("Infected sites \n(cumulative)") +
    geom_line(data = scenario_smooth, aes(x = x, y = y)) +
    theme_light()

  # create plot folder to save plot if does not exist
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

