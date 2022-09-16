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
stateCosts <- function(data, state, site_types){
  # Select state
  if(state == "fallow"){
    state_codes <- c(4, 5,
                     14, 15,
                     24, 25,
                     34, 35)
    # Add a cull state (same cost as fallow)
    data$cull_state <- NA
    for(j in 1:nrow(data)){
      if(data$state[j] %in% c(4, 14, 24, 34) &&
         data$state[j-1] %in% c(2, 8, 12, 18, 22, 28, 32, 38)){
        data$cull_state[j-1] <- "Y"
      }
    }
    state_summary <- data[cull_state == "Y" | state %in% state_codes]
  } else if(state == "no_manage"){
    state_codes <- c(10, 30)
    # Filter data by state
    state_summary <- data[state %in% state_codes]
  } else if(state == "contact_trace"){
    state_codes <- c(1, 7,
                     11, 17,
                     21, 27,
                     31, 37)
    # Filter data by state
    state_summary <- data[state %in% state_codes]
  } else if(state == "catchment_control"){
    state_codes <- c(20, 21,
                     24, 25,
                     26, 27)
    # Filter data by state
    state_summary <- data[state %in% state_codes]
  }
  # Get daily cost for that stage
  daily_cost <- data.table(daily_cost)
  state_daily_cost <- daily_cost[stage == state]
  # Calculate duration costs for each site and sim
  sims <- unique(state_summary[, sim_no])
  costs <- data.table()
  length(costs) <- length(sims)
  for(k in 1:length(sims)){ # Loop over sim_no
    for(i in 1:length(site_types)){ # Loop over site type
      if(!(state %in% c("fallow", "no_manage", "contact_trace", "catchment_control"))) +
        stop("You have entered an incorrect state type. Please choose from fallow, no_manage, contact_trace or catchment_controls")
      type <- site_types[[i]] # Set site type
      daily_site_cost <- state_daily_cost[site_type == type]
      state_summary_by_sim <- state_summary[sim_no == sims[k]]
      # Calculate the cost over the duration of the simulation
      # duration_cost = site type (1) * duration of time in the state * daily cost
      duration_cost <- (state_summary_by_sim[, ..type] * state_summary_by_sim[, "t_total"]) * daily_site_cost$farm_cost_per_day
      total_duration_cost <- as.data.frame(sum(duration_cost)) # Calculate the total cost over the duration
      total_duration_cost$sim_no <- sims[k] # Add simulation number
      costs <- rbind(costs, total_duration_cost)
    }
    print(sims[k])}

  # Make into data frame
  state_costs <- cbind(site_types, costs)
  colnames(state_costs)[2] <- "duration_cost"

  # Calculate total cost over the course of a simulation
  sim_cost_summary <- tidyr::pivot_wider(state_costs, names_from = site_types,
                                         values_from = duration_cost) # Reformat
  sim_cost_summary$total_cost <- rowSums(sim_cost_summary[, -1], na.rm = T) # Total cost [, -1] so as not to include the sim_no in total
  colnames(sim_cost_summary)[16] <- paste0(state, "_total_cost")
  sim_cost_summary <- sim_cost_summary[, c("sim_no", paste0(state, "_total_cost"))]
  cost_output <- list(state_costs, sim_cost_summary)
  names(cost_output) <- c("full_state_costs", "summary_state_costs")
  return(cost_output)
}
