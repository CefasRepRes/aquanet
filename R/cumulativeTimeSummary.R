#' cumulativeTimeSummary
#'
#' Creates a summary of the cumulative number of sites infected by day for a given
#' scenario. Used to create time plots using the timePlots function.
#'
#' @param scenario_name (class string) the name of the scenario being loaded.
#' @import here
#' @import data.table
#' @return scenario_summary
#' @export
#'

cumulativeTimeSummary <- function(scenario_name){
  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  t_simplified <- cumulative_no_infected_sites <- sim_no <- x <- y <- . <- NULL
  # load results
  scenario <- data.table(aquanet::loadResultsSummary(scenario_name))
  scenario_summary <- scenario[, .(t, cumulative_no_infected_sites, sim_no)]
  class(scenario_summary)

  # create plot folder to save plot if does not exist
  if (!file.exists(here::here("outputs","plots"))){
    dir.create(here::here("outputs","plots"))
    cat("Directory 'plots' created\n")
  } else {
    cat("Directory 'plots' already exists\n")
  }

  # save as csv
  write.csv(scenario_summary, here::here("outputs",
                                         "plots",
                                         paste0("time_summary_", scenario_name, ".csv")),
            row.names = FALSE)
  return(scenario_summary)
}
