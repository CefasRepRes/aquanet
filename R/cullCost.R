#' cullCost
#'
#' Calculates a one-off cost to both the site and the competent authority in the
#' case of a site needing to be culled. Returns a data frame the
#' cost to cull across each simulation.
#'
#' @param farm_data (class data.frame) the `time_summary` data frame created from the
#' `timePerStage` function, for farms only.
#' @param non_farm_data (class data.frame) the `time_summary` data frame created from the
#' `timePerStage` function, for non-farms (fisheries) only.
#' @param cull_costs (class data.frame) of the costs to both the site and the
#' competent authority for each site type.
#' @param site_types (class list) a list of possible site types.
#'
#' @return (class data.frame) full_cull_cost_sim
#' @export
#'
#' @import data.table
#'
cullCost <- function(farm_data, non_farm_data, cull_costs, site_types){
  # When a site enters a cull state enter a one-off cost
  farm_data <- data.table(farm_data)
  non_farm_data <- data.table(non_farm_data)
  cull <- farm_data[cull_state == "Y"]
  sims <- unique(farm_data[, sim_no])
  # Loop over simulations and add the one-off cull cost, depending on site type
  full_cull_cost_farm <- data.table()
  system.time({
    for(k in 1:length(sims)){
      for(i in 1:length(site_types)){
        type <- site_types[[i]]
        cull <- data.table(cull)
        cull_by_sim <- cull[sim_no == sims[k]]
        cull_cost <- data.table(cull_cost)
        filter_cull_cost <- cull_cost[site_type == type]
        # Separate costs to the farm and the FHI (competent authority)
        farm_cull_cost <- sum(cull_by_sim[, ..type]) * filter_cull_cost$cull_cost_farm
        fhi_cull_cost <- sum(cull_by_sim[, ..type]) * filter_cull_cost$cull_cost_fhi
        # Combine into single farm_data frame
        comb_cull_cost <- data.table(cull_cost_site = farm_cull_cost,
                                     cull_cost_fhi = fhi_cull_cost,
                                     sim_no = sims[k])
        # Save results
        full_cull_cost_farm <- rbind(full_cull_cost_farm, comb_cull_cost)
      }
      print(sims[k])
    }})
  # Calculate the cost for fisheries (not by type)
  non_farm_cull <- non_farm_data[cull_state == "Y"]
  non_farm_cull_per_sim <- non_farm_cull[, .N, by = sim_no]
  non_farm_cull_per_sim$cull_cost_fhi_fisheries <- non_farm_cull_per_sim$N * 18225
  non_farm_cull_per_sim$cull_cost_site_fisheries <- NA
  non_farm_cull_per_sim <- non_farm_cull_per_sim[, !"N"]
  # Summarise per simulation
  full_cull_cost_sim_farm <- full_cull_cost_farm[, by = sim_no,
                                                 .(cull_cost_site_farms = sum(cull_cost_site, na.rm = T),
                                                   cull_cost_fhi_farms = sum(cull_cost_fhi, na.rm = T))]
  full_cull_cost_sim <- merge(full_cull_cost_sim_farm, non_farm_cull_per_sim,
                              all = TRUE, by = "sim_no")
  full_cull_cost_sim$total_cull_cost <- rowSums(full_cull_cost_sim[, -1], na.rm = T) # Total cost [, -1] so as not to include the sim_no in total
  return(full_cull_cost_sim)
}
