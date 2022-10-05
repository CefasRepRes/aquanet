#' epidemicPeak
#'
#' Calculates the epidemic peak: the number of infected sites at any one time step in the scenario.
#'
#' @param results (class data.frame) A data frame created from `aquanet::loadResultsSummary` that
#' contains:
#' 1. `batch_no` the batch number
#' 2. `k` the run number within the batch
#' 3. `t` the model time
#' 4. `t_diff` the difference between the current time and the time at the previous timestep (the
#' duration of the previous timestep)
#' 5. `sim_no` the simulation number
#' 6. `rate_type` the transition type
#' 7. `no_controlled_catchments` the number of catchments under controls in that timestep
#' 8. `cumulative_no_infected_sites` the cumulative number of infected sites in the simulation
#' Plus the number of sites in each state (1:39) plus redundancy (40:42)
#'
#' @return (class data.table) summary of epidemic infections (measured in number of sites infected):
#' 1. `mean_infections` the mean epidemic infections for the scenario
#' 2. `sd_infections` the standard deviation of the epidemic infections for the scenario
#' 3. `min_infections` minimum epidemic infections
#' 4. `max_infections` maximum epidemic infections
#' 5. `median_infections` medium epidemic infections
#' 6. `q05_infections` the 5% quartile for epidemic infections
#' 7. `q95_infections` the 95% quartile for epidemic infections
#' 9. `no_iter` number of iterations/simulations run
#'
#' @export
#'
#' @importFrom stats median
#' @importFrom stats sd
#'
epidemicPeak <- function(results) {

  # convert results into data.table
  results <- data.table(results)

  # Calcultate infected sites at each timestep
  results$infected_sites <- rowSums(results[,c(10:19, 30:39)])

  # remove sim_no 0 (due to overallocation)
  valid_results <- results[sim_no != 0] # epidemic size

  no_inf <- valid_results[, c("sim_no", "infected_sites")][ # select sim_no and cumulative infected sites
    , by = sim_no, .(max_inf = max(infected_sites))][ # get maximum infected sites per sim_no
      , c("sim_no", "max_inf")] # select sim_no and max_inf

  no_inf <- unique(no_inf)

  # statistics ----
  mean_infections <- mean(no_inf[, max_inf])
  sd_infections <- sd(no_inf[, max_inf])
  min_infections <- min(no_inf[, max_inf])
  max_infections <- max(no_inf[, max_inf])
  median_infections <- median(no_inf[, max_inf])
  q5_infections <- quantile(no_inf[, max_inf], 0.05)
  q95_infections <- quantile(no_inf[, max_inf], 0.95)

  # get number of iterations
  iterations <- max(results[, sim_no])

  # put into nice table
  scenario_results <- data.table::data.table(mean_infections = mean_infections,
                                             sd_infections = sd_infections,
                                             min_infections = min_infections,
                                             max_infections = max_infections,
                                             median_infections = median_infections,
                                             q05_infections = q5_infections,
                                             q95_infections = q95_infections,
                                             percent_die_out = die_out,
                                             no_iter = iterations)
  return(scenario_results)
}
