#' cullCost
#'
#' Calculates a one-off cost to both the site and the competent authority in the case of a site
#' needing to be culled. Returns a data frame the cost to cull across each simulation.
#'
#' @param farm_data (class data.frame) the `time_summary` data frame created from the
#' `aquanet::importAndCondense()` function, for farms only.
#'
#' @param non_farm_data (class data.frame) the `time_summary` data frame created from the
#' `aquanet::importAndCondense` function, for non-farms (fisheries) only.
#'
#' @param cull_costs (class data.frame) of the costs to both the site and the competent authority
#' for each site type.
#'
#' @param site_types (class character)  a vector of possible site types.
#'
#' @return (class data.table) the cost to cull across each simulation.
#'
#' @export
#'
#' @import data.table

cullCost <- function(farm_data,
                     #non_farm_data,
                     cull_costs,
                     site_types) {

  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  value <- number_culled <- site_type <- cull_cost_ca <- cull_state <- NULL
  cull_cost_site <- sim_no <- max_cull_cost_farm <- cull_cost_farm <- site_id <- max_cull_cost_ca <- . <- NULL

  # convert input data frames to data tables
  farm_data <- data.table::data.table(farm_data)
  #non_farm_data <- data.table::data.table(non_farm_data)
  cull_cost <- data.table::data.table(cull_cost)

  # calculate farm cull costs ----
  # convert farm data to long format
  farm_data_long <- data.table::melt(farm_data,
                                     id.vars = c("site_id", "modelID", "state", "group",
                                                 "sim_no", "timeID", "t_total", "t", "trans_type",
                                                 "farm_vector", "cull_state"),
                                     measure.vars = c(site_types))

  # rename columns for readability
  data.table::setnames(farm_data_long, old = "variable", new = "site_types")

  # multiply value column indicate whether site by cull state (grouped by sim_no)
  farm_data_long[ , value := value * cull_state, ]

  #######################################################################################################################

  # Make sure only one cull cost is applied to site: pick the largest cull cost

  # Only select if value =1 (aka if the category is true) and cull state is TRUE
  farm_data_long_cull <- farm_data_long[value == 1]

  # merge with cull cost
  farm_data_long_cull_costs <- merge(farm_data_long_cull, cull_cost, by.x='site_types', by.y = 'site_type')

  # Convert as numeric
  farm_data_long_cull_costs$cull_cost_farm <- as.numeric(farm_data_long_cull_costs$cull_cost_farm)
  farm_data_long_cull_costs$cull_cost_ca<- as.numeric(farm_data_long_cull_costs$cull_cost_ca)

  # only select maximum cull cost for site (based on site types which is true)- Farm
  farm_data_long_cull_costs[, max_cull_cost_farm := max(cull_cost_farm, na.rm = TRUE), by = site_id]


  # only select maximum cull cost for site (based on site types which is true)- CA
  farm_data_long_cull_costs[, max_cull_cost_ca := max(cull_cost_ca, na.rm = TRUE), by = site_id]

  # Only keep 1 entry for if same site_id, timeID and sim number
  farm_data_long_cull_costs_unique <- farm_data_long_cull_costs[, .SD[1], by = .(site_id, sim_no, timeID)]


  # calculate the total cull costs costs per sim_no
  #farm_cull_costs <- farm_data_long_cull_costs[, cull_cost_farm_by_sim:= sum(max_cull_cost_farm, na.rm = TRUE), by = sim_no]

  full_cull_cost_sim_farm <- farm_data_long_cull_costs_unique[ ,
                                              .(cull_cost_ca_farms = sum(max_cull_cost_ca, na.rm = TRUE),
                                                cull_cost_farms = sum(max_cull_cost_farm, na.rm = TRUE)),
                                              by = .(sim_no)]



  #######################################################################################################################

  # commented out as in FC1216 we don't have fishery cost
  # calculate fishery cull costs (not by type) ----
  #non_farm_cull <- non_farm_data[cull_state == TRUE]
  #non_farm_cull_per_sim <- non_farm_cull[, .N, by = sim_no]
  #non_farm_cost <- mean(cull_cost[grepl("Fish", site_type)]$cull_cost_ca)
  #non_farm_cull_per_sim$cull_cost_ca_fisheries <- non_farm_cull_per_sim$N * non_farm_cost
  #non_farm_cull_per_sim$cull_cost_site_fisheries <- NA
  #non_farm_cull_per_sim <- non_farm_cull_per_sim[, !"N"]


  # combine cull cost results farms and fisheries ----
  #full_cull_cost_sim <- merge(full_cull_cost_sim_farm,
                             # non_farm_cull_per_sim,
                             # all = TRUE,
                             # by = "sim_no")

  # Total cost per simulation for culling of farms and fisheries (to site and competent authority)
  full_cull_cost_sim_farm$total_cull_cost <- rowSums(full_cull_cost_sim_farm[, -"sim_no"], na.rm = TRUE)

  # change cull method- not as site type as multiple types and avoid duplicates
  # Combine with breakdown by site type
  #full_cull_cost <- list("cull_cost_by_sim" = full_cull_cost_sim,
   #                      "cull_cost_farm_by_type" = farm_cull_costs)

  return(full_cull_cost_sim_farm)
}
