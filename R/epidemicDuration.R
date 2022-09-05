#' epidemicDuration
#'
#' @param results (class data.frame) A data frame created from `loadResultsSummary`
#' Contains:
#' 1. `batch_no` the batch number
#' 2. `k` the run number within the batch
#' 3. `t` the model time
#' 4. `t_diff` the difference between the current time and the time at the previous timestep
#' (the duration of the previous timestep)
#' 5. `sim_no` the simulation number
#' 6. `rate_type` the transition type
#' 7. `no_controlled_catchments` the number of catchments under controls in that timestep
#' 8. `cumulative_no_infected_sites` the cumulative number of infected sites in the simulation
#' Plus the number of sites in each state (1:39) plus redundancy (40:42)
#'
#' @return (class data.table) summary of epidemic duration (measured in days):
#' 1. `mean_duration` the mean epidemic duration for the scenario
#' 2. `sd_duration` the standard deviation of the epidemic duration for the scenario
#' 3. `min_duration` minimum epidemic duration
#' 4. `max_duration` maximum epidemic duration
#' 5. `median_duration` medium epidemic duration
#' 6. `q05_duration` the 5% quartile for epidemic duration
#' 7. `q95_duration` the 95% quartile for epidemic duration
#' 8. `percent_time_out` the percentage of simulations for which the epidemic did not
#' last longer than 5 years (1800 model days)
#' 9. `no_iter` numer of iterations/simulations run
#'
#' @export
#'
#' @importFrom stats median
#' @importFrom stats sd
#'
epidemicDuration <- function(results){
  # Convert results into data.table
  results <- data.table(results)
  # Remove sim_no 0 (due to overallocation)
  valid_results <- results[sim_no != 0]
  # Epidemic duration
  no_days <- valid_results[, c("sim_no", "t")][ # Select sim_no and t
    , by = sim_no, .(max_t = max(t))][ # Get maximum time per sim_no
      , c("sim_no", "max_t")] # Select sim_no and max_t
  # Remove duplicates
  no_days <- unique(no_days)
  # Statistics
  mean_duration <- mean(no_days[, max_t])
  sd_duration <- sd(no_days[, max_t])
  min_duration <- min(no_days[, max_t])
  max_duration <- max(no_days[, max_t])
  median_duration <- median(no_days[, max_t])
  q5_duration <- quantile(no_days[, max_t], 0.05)
  q95_duration <- quantile(no_days[, max_t], 0.95)
  # Calculate percent of timed out simulations
    # Time out is where the epidemic doesn't last more than 5 years
  time_out <- (sum(no_days[, max_t] < 5*360)/3000)*100
  # Get number of iterations
  iterations <- max(results[, sim_no])
  # Put into nice table
  scenario_results <- data.table::data.table(mean_duration = mean_duration,
                                             sd_duration = sd_duration,
                                             min_duration = min_duration,
                                             max_duration = max_duration,
                                             median_duration = median_duration,
                                             q05_duration = q5_duration,
                                             q95_duration = q95_duration,
                                             percent_time_out = time_out,
                                             no_iter = iterations)
  return(scenario_results)
}
