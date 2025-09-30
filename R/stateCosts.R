#' stateCosts
#'
#' Calculates the total duration costs for each simulation across fallow, contact tracing,
#' catchment control and unmanaged disease scenarios.
#'
#' @param data (class data.frame) output of timePerStage. Contains the site ID,
#' state, simulation number, total time, and the logical vector of site types.
#'
#' @param state (class character) describes the state for which the costs will be
#' calculated. Accepted inputs are: `"fallow"` in a fallow state (infected or uninfected),
#' `"no_manage"` infected in an unmanaged state, `"contact_trace"` in a contact tracing state
#' (infected or uninfected), or `"catchment_control"` uninfected site under catchment-level
#' controls.
#'
#' @param site_types (class character) a vector of possible site types.
#'
#' @return (class list) of length 2 containing:
#' 1. (class data.table) a full breakdown of daily costs by site type and simulation.
#' 2. (class data.table) a summary data frame containing the simulation number and the cost
#' incurred by sites being in that state.
#'
#' @export
#'
#' @import data.table
#'
# define column names used with data.table syntax

stateCosts <- function(data,
                       state,
                       site_types){

  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  value <- value_2 <-  t_total <- timeID <- time_in_state <- total_duration <- duration_cost <- farm_cost_per_unit <- NULL
  cull_state <- sim_no <- stage <- . <- number_of_months <- value_t <- max_no_manage_cost <-site_id <- NULL


  # filter data depending on input state
  if(state == "fallow"){
    state_codes <- c(4, 5, 14, 15, 24, 25, 34, 35)
    state_summary <- data[cull_state == TRUE | state %in% state_codes]

  } else if(state == "no_manage"){
    state_codes <- 10
    state_summary <- data[state %in% state_codes]


  } else if(state == "catchment_control"){
    state_codes <- c(20, 21, 26, 27)
    state_summary <- data[state %in% state_codes]

  }

  # get unit cost for that stage
  unit_cost <- data.table(unit_cost)
  state_unit_cost <- unit_cost[stage == state]

  if (!(state %in% c("fallow", "no_manage", "contact_trace", "catchment_control"))) +
    stop("You have entered an incorrect state type. Please choose from fallow, no_manage, contact_trace or catchment_controls")

  # convert to long format combining site_types into 'variable' column with 'value' column of 0/1
  state_summary_long <- data.table::melt(state_summary,
                                         id.vars = c("site_id", "modelID", "state", "group",
                                                     "sim_no", "timeID", "t_total", "t", "trans_type",
                                                     "farm_vector", "cull_state"),
                                         measure.vars = c(site_types))

  # multiply site_type 'value' by 't_total' to determine time spent for each site type
  state_summary_long[ , value_t := value * t_total, ]

  # rename columns to something more readable
  data.table::setnames(state_summary_long, old = "variable", new = "site_types")
  data.table::setnames(state_summary_long, old = "value_t", new = "time_in_state")

  # sum the 'time_in_state' by each of the 'site_types' (group by sim_no)
  aggregated <- state_summary_long[ , .(total_duration =  sum(time_in_state)), by = .(sim_no, site_types)]


  ######################################################################
  # In your economic code only use 'no_manage' if no controls scenario
  # one cost applied for simulation every time site is infected. Cost for different sides of business applied e.g. if
  #site is a small hatchery and small ongrower both 'no mange' costs are applied

  if (state == "no_manage") {

    state_summary_long <- state_summary_long[value == 1]
    no_controls_costs <- unit_cost[stage == 'no_manage']
    state_summary_long_costs <- merge(state_summary_long, no_controls_costs, by.x='site_types', by.y = 'site_type')

    sim_cost_summary <- state_summary_long_costs[, .(total_cost = sum(farm_cost_per_unit, na.rm = TRUE)),
                                                 by = .(sim_no)]

    # rename columns to incorporate state
    data.table::setnames(sim_cost_summary, old = "total_cost", new = paste0(state, "_total_cost"))

    # output list of results and return
    cost_output <- list("summary_state_costs" = sim_cost_summary)


    ######################################################################
    # No catchment costs are applied per month. Cumulative costs e.g. cost for all sides of business.

  } else if (state == "catchment_control") {
    # Calculate number of months in state
    aggregated[, number_of_months := total_duration / 30]

    # Round number_of_months based on the decimal point
    aggregated[, number_of_months := ifelse(number_of_months %% 1 >= 0.5,
                                            ceiling(number_of_months),
                                            floor(number_of_months))]

    # Calculate duration_cost using number_of_months
    # multiply total 'number_if_months' by corresponding site_type monthly costs to get state_costs
    state_costs <- aggregated[ ,
                               .(site_types = site_types,
                                 number_of_months = number_of_months,
                                 duration_cost = number_of_months * state_unit_cost$farm_cost_per_unit[eval(site_types)]),
                               by = .(sim_no)]

    # aggregate to get cost per simulation (across all site_types)
    sim_cost_summary <- state_costs[ , .(total_cost = sum(duration_cost, na.rm = T)) , by = .(sim_no)]
    data.table::setnames(state_costs, old = "number_of_months", new = paste0(state, "_number_of_months"))
    data.table::setnames(state_costs, old = "duration_cost", new = paste0(state, "_total_duration_cost"))
    # rename columns to incorporate state
    data.table::setnames(sim_cost_summary, old = "total_cost", new = paste0(state, "_total_cost"))

    # output list of results and return
    cost_output <- list("full_state_costs" = state_costs,
                        "summary_state_costs" = sim_cost_summary)

  } else {
    # Fallow costs, cost applied per day/time under state 'fallow'. All sides of business applied.
    # multiply 'total_duration' by corresponding site_type daily costs to get state_costs
    state_costs <- aggregated[ ,
                               .(site_types = site_types,
                                 total_duration = total_duration,
                                 duration_cost = total_duration * state_unit_cost$farm_cost_per_unit[eval(site_types)]),
                               by = .(sim_no)]

    # aggregate to get cost per simulation (across all site_types)
    sim_cost_summary <- state_costs[ , .(total_cost = sum(duration_cost, na.rm = T)) , by = .(sim_no)]
    data.table::setnames(state_costs, old = "total_duration", new = paste0(state, "_total_duration"))
    data.table::setnames(state_costs, old = "duration_cost", new = paste0(state, "_total_duration_cost"))
    # rename columns to incorporate state
    data.table::setnames(sim_cost_summary, old = "total_cost", new = paste0(state, "_total_cost"))

    # output list of results and return
    cost_output <- list("full_state_costs" = state_costs,
                        "summary_state_costs" = sim_cost_summary)
  }

  return(cost_output)

}
