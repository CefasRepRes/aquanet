#' transmissionRouteProportions
#'
#' Get the proportions of infections via each possible mechanism for each scenario.
#' Possible mechanisms are:
#' 1. LFM = live fish movement
#' 2. RB = river-based transmission
#' 3. SDM = short-distance mechanical
#' 4. DIM = distance-independent mechanical (random spillover)
#'
#' @param scenario_name The name of the scenario to be checked.
#'
#' @import data.table
#' @return (class data.table) A summary of the infections per scenario, in terms of count and percentage
#' @export
#'
transmissionRouteProportions <- function(scenario_name) {
  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  variable <- value <- trans_type <- total <- sim_no <- infection_by <- . <- NULL

  # read in summarised parquet file from aquanet::importAndCondense
  time_summary <- arrow::read_parquet(here::here("outputs",
                                                 scenario_name,
                                                 "economics",
                                                 paste0(scenario_name, "-details-condensed.parquet")))

  # summarise number of different infection events per simulation
  infection_events_per_sim <- time_summary[ ,
                                            .(LFM = sum(trans_type == 0),
                                              RB = sum(trans_type == 10),
                                              SDM = sum(trans_type == 14),
                                              DIM = sum(trans_type == 11)),
                                            .(sim_no)]

  # convert to long format
  infection_events_long <- data.table::melt(infection_events_per_sim,
                                          id.vars = "sim_no")

  # get the total number of infections via each route
  infection_events_summary <- setDT(infection_events_long)[ , .(infection_by = variable, total = sum(value)), by = variable]

  # calculate proportion of infections via each route
  infection_events_summary <- infection_events_summary[ , .(infection_by = infection_by,
                                                            total = total,
                                                            proportion = total/ sum(total)), ]

  # return summary
  return(infection_events_summary)
}
