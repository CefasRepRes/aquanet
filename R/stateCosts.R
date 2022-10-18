#' stateCosts
#'
#' Calculates the total duration costs for each simulation across fallow, contact tracing,
#' catchment control and unmanaged disease scenarios. Returns (1) a full breakdown of daily
#' costs by site type and simulation and (2) a summary data frame containing
#' the simulation number and the cost incurred by sites being in that state.
#'
#' @param data (class data.frame) output of timePerStage. Contains the site ID,
#' state, simulation number, total time, and the logical vector of site types.
#' @param state (class character) describes the state for which the costs will be
#' calculated. Can be:
#' 1. `fallow` in a fallow state (infected or uninfected)
#' 2. `no_manage` infected in an unmanaged state
#' 3. `contact_trace` in a contact tracing state (infected or uninfected)
#' 4. `catchment_control` uninfected site under catchment-level controls
#' @param site_types (class list) a list of possible site types
#'
#' @return (class list) cost_output
#' @export
#'
#' @import data.table
#' @import tidyr
#'
stateCosts <- function(data,
                       state,
                       site_types){

  # define column names used with data.table syntax
    # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  value <- t_total <- time_in_state <- total_duration <- duration_cost <- NULL

  # filter data depending on input state
  if(state == "fallow"){
    state_codes <- c(4, 5, 14, 15, 24, 25, 34, 35)
    state_summary <- data[cull_state == "Y" | state %in% state_codes]

  } else if(state == "no_manage"){
    state_codes <- c(10, 30)
    state_summary <- data[state %in% state_codes]

  } else if(state == "contact_trace"){
    state_codes <- c(1, 7, 11, 17, 21, 27, 31, 37)
    state_summary <- data[state %in% state_codes]

  } else if(state == "catchment_control"){
    state_codes <- c(20, 21, 24, 25, 26, 27)
    state_summary <- data[state %in% state_codes]

  }

  # get daily cost for that stage
  daily_cost <- data.table(daily_cost)
  state_daily_cost <- daily_cost[stage == state]

  if (!(state %in% c("fallow", "no_manage", "contact_trace", "catchment_control"))) +
    stop("You have entered an incorrect state type. Please choose from fallow, no_manage, contact_trace or catchment_controls")

  # convert to long format combining site_types into 'variable' column with 'value' column of 0/1
  state_summary_long <- data.table::melt(state_summary,
                                         id.vars = c("site_id", "modelID", "state", "group",
                                                     "sim_no", "timeID", "t_total", "t", "trans_type",
                                                     "farm_vector", "row_sums", "cull_state"),
                                         measure.vars = c(site_types))

  # multiply site_type 'value' by 't_total' to determine time spent for each site type
  state_summary_long[ , value := value * t_total, ]

  # rename columns to something more readable
  data.table::setnames(state_summary_long, old = "variable", new = "site_types")
  data.table::setnames(state_summary_long, old = "value", new = "time_in_state")

  # sum the 'time_in_state' by each of the 'site_types' (group by sim_no)
  aggregated <- state_summary_long[ , .(total_duration =  sum(time_in_state)), by = .(sim_no, site_types)]

  # multiply 'total_duration' by corresponding site_type daily costs to get state_costs
  state_costs <- aggregated[ ,
                             .(site_types = site_types,
                               total_duration = total_duration,
                               duration_cost = total_duration * state_daily_cost$farm_cost_per_day[eval(site_types)]),
                             by = .(sim_no)]

  # aggregate to get cost per simulation (across all site_types)
  sim_cost_summary <- state_costs[ , .(total_cost = sum(duration_cost, na.rm = T)) , by = .(sim_no)]

  # rename columns to incorporate state
  data.table::setnames(sim_cost_summary, old = "total_cost", new = paste0(state, "_total_cost"))

  data.table::setnames(state_costs, old = "total_duration", new = paste0(state, "_total_duration"))
  data.table::setnames(state_costs, old = "duration_cost", new = paste0(state, "_total_duration_cost"))

  # output list of results and return
  cost_output <- list("full_state_costs" = state_costs,
                      "summary_state_costs" = sim_cost_summary)
  return(cost_output)

}
