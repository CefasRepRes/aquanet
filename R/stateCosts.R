#' stateCosts
#'
#' Calculates the total duration costs for each simulation across fallow, contact tracing,
#' catchment control and unmanaged disease scenarios. Returns a data frame containing
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
#' @return (class data.frame) sim_cost_summary
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
  } else if(state == "no_manage"){
    state_codes <- c(10, 30)
  } else if(state == "contact_trace"){
    state_codes <- c(1, 7,
                     11, 17,
                     21, 27,
                     31, 37)
  } else if(state == "catchment_control"){
    state_codes <- c(21, 24,
                     25, 26,
                     27)
  }
  # Filter data by state
  state_summary <- data[state %in% state_codes]
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
  return(sim_cost_summary)
}
