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
cullCost <- function(farm_data,
                     non_farm_data,
                     cull_costs,
                     site_types) {

  # define column names used with data.table syntax
    # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  value <- number_culled <- NULL

  # convert input data frames to data tables
  farm_data <- data.table::data.table(farm_data)
  non_farm_data <- data.table::data.table(non_farm_data)
  cull_cost <- data.table::data.table(cull_cost)

  # calculate farm cull costs ----
  # convert farm data to long format
  farm_data_long <- data.table::melt(farm_data,
                                     id.vars = c("site_id", "modelID", "state", "group",
                                                 "sim_no", "timeID", "t_total", "t", "trans_type",
                                                 "farm_vector", "row_sums", "cull_state"),
                                     measure.vars = c(site_types))

  # rename columns for readability
  data.table::setnames(farm_data_long, old = "variable", new = "site_types")

  # multiply value column indicate whether site by cull state (grouped by sim_no)
  farm_data_long[ , value := value * cull_state, ]

  # count the number of culls by site_type (grouped by sim_no)
  farm_cull_counts <- farm_data_long[ , .(number_culled = sum(value)), by = .(sim_no, site_types)]

  # get the costs of culling farms by site_type (grouped by sim_no)
  farm_cull_costs <- farm_cull_counts[ ,
                                       .(site_types = site_types,
                                         cull_cost_site = number_culled * cull_cost[site_type == eval(site_types)]$cull_cost_farm,
                                         cull_cost_fhi = number_culled * cull_cost[site_type == eval(site_types)]$cull_cost_fhi),
                                       by = .(sim_no)]

  # calculate the total cull costs costs per sim_no
  full_cull_cost_sim_farm <- farm_cull_costs[ ,
                                              .(cull_cost_site_farms = sum(cull_cost_fhi, na.rm = TRUE),
                                                cull_cost_fhi_farms = sum(cull_cost_site, na.rm = TRUE)),
                                              by = .(sim_no)]


  # calculate fishery cull costs (not by type) ----
  non_farm_cull <- non_farm_data[cull_state == TRUE]
  non_farm_cull_per_sim <- non_farm_cull[, .N, by = sim_no]
  non_farm_cull_per_sim$cull_cost_fhi_fisheries <- non_farm_cull_per_sim$N * 18225
  non_farm_cull_per_sim$cull_cost_site_fisheries <- NA
  non_farm_cull_per_sim <- non_farm_cull_per_sim[, !"N"]


  # combine cull cost results farms and fisheries ----
  full_cull_cost_sim <- merge(full_cull_cost_sim_farm,
                              non_farm_cull_per_sim,
                              all = TRUE,
                              by = "sim_no")

  # Total cost per simulation for culling of farms and fisheries (to site and competent authority)
  full_cull_cost_sim$total_cull_cost <- rowSums(full_cull_cost_sim[, -"sim_no"], na.rm = TRUE)

  return(full_cull_cost_sim)
}
