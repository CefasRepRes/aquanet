#' cullCost
#'
#' Calculates a one-off cost to both the site and the competent authority in the
#' case of a site needing to be culled. Returns a data frame the
#' cost to cull across each simulation.
#'
#' @param data (class data.frame) the `time_summary` data frame created from the
#' `timePerStage` function.
#' @param cull_costs (class data.frame) of the costs to both the site and the
#' competent authority for each site type.
#' @param site_types (class list) a list of possible site types.
#'
#' @return (class data.frame) full_cull_cost_sim
#' @export
#'
#' @import data.table
#'
cullCost <- function(data, cull_costs, site_types){
  # When a site enters a fallow state enter a one-off cost
  data <- data.table(data)
  fallow <- data[state %in% c(4,
                              14,
                              24,
                              34)]
  sims <- unique(data[, sim_no])
  # Loop over simulations and add the one-off fallow cost, depending on site type
  full_cull_cost <- data.table()
  system.time({
    for(k in 1:length(sims)){
      for(i in 1:length(site_types)){
        type <- site_types[[i]]
        fallow <- data.table(fallow)
        fallow_by_sim <- fallow[sim_no == sims[k]]
        cull_cost <- data.table(cull_cost)
        filter_cull_cost <- cull_cost[site_type == type]
        # Separate costs to the farm and the FHI (competent authority)
        farm_cull_cost <- sum(fallow_by_sim[, ..type]) * filter_cull_cost$cull_cost_farm
        fhi_cull_cost <- sum(fallow_by_sim[, ..type]) * filter_cull_cost$cull_cost_fhi
        # Combine into single data frame
        comb_cull_cost <- data.table(cull_cost_farm = farm_cull_cost,
                                     cull_cost_fhi = fhi_cull_cost,
                                     sim_no = sims[k])
        # Save results
        full_cull_cost <- rbind(full_cull_cost, comb_cull_cost)
      }
      print(sims[k])
    }})

  # Summarise per simulation
  full_cull_cost_sim <- full_cull_cost[, by = sim_no,
                                       .(total_cull_cost_farm = sum(cull_cost_farm, na.rm = T),
                                         total_cull_cost_fhi = sum(cull_cost_fhi, na.rm = T))]
  return(full_cull_cost_sim)
}
