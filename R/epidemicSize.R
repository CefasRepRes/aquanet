#' epidemicSize
#'
#' This function summarises the simulation results to return statistics related to the size of the
#' epidemic, including mean and median infection numbers and percentage die out.
#'
#' @param results (class data.frame) A data frame created from `aquanet::loadResultsSummary()` that
#' contains:
#' 1. `batch_no` the batch number.
#' 2. `k` the run number within the batch.
#' 3. `t` the model time.
#' 4. `t_diff` the difference between the current time and the time at the previous timestep (the
#' duration of the previous timestep).
#' 5. `sim_no` the simulation number.
#' 6. `rate_type` the transition type.
#' 7. `no_controlled_catchments` the number of catchments under controls in that timestep.
#' 8. `cumulative_no_infected_sites` the cumulative number of infected sites in the simulation.
#' Plus the number of sites in each state (1:39) plus redundancy (40:42)
#'
#' @param summary (class logical) A logical to return either a summary (T) or the number of infections
#' per scenario (F). F is the default.
#'
#' @return (class data.table)
#'
#' If summary = F, the simulation number `sim_no` and the total number of infected sites `tot_inf`.
#'
#' If summary = T, summary of epidemic infections (measured in number of sites infected):
#' 1. `mean_infections` the mean epidemic infections for the scenario.
#' 2. `sd_infections` the standard deviation of the epidemic infections for the scenario
#' 3. `min_infections` minimum epidemic infections.
#' 4. `max_infections` maximum epidemic infections.
#' 5. `median_infections` medium epidemic infections.
#' 6. `q05_infections` the 5% quartile for epidemic infections.
#' 7. `q95_infections` the 95% quartile for epidemic infections.
#' 8. `percent_die_out` the percentage of simulations for which the epidemic did not infect more
#' than 5 sites in total.
#' 9. `no_iter` number of iterations per simulation run.
#'
#' @export
#'
#' @importFrom stats median
#' @importFrom stats sd
#'
epidemicSize <- function(results, summary = F) {

  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  cumulative_no_infected_sites <- tot_inf <- sim_no <- . <- NULL

  # convert results into data.table
  results <- data.table(results)

  # remove sim_no 0 (due to overallocation)
  valid_results <- results[sim_no != 0] # epidemic size

  no_inf <- valid_results[, c("sim_no", "cumulative_no_infected_sites")][ # select sim_no and cumulative infected sites
    , by = sim_no, .(tot_inf = max(cumulative_no_infected_sites))][ # get maximum cumulative infected sites per sim_no
      , c("sim_no", "tot_inf")] # select sim_no and tot_inf

  no_inf <- unique(no_inf)

  # add a marker where the epidemic died out
  # died out is where the epidemic infects fewer than 5 sites in total
  for(i in 1:nrow(no_inf)){
    no_inf[i, die_out := if(no_inf[i, tot_inf] < 5){TRUE} else {FALSE}]
  }

  # summary
  if(summary == TRUE){
    # statistics ----
    mean_infections <- mean(no_inf[ , tot_inf])
    sd_infections <- sd(no_inf[ , tot_inf])
    min_infections <- min(no_inf[ , tot_inf])
    max_infections <- max(no_inf[ , tot_inf])
    median_infections <- median(no_inf[ , tot_inf])
    q5_infections <- quantile(no_inf[ , tot_inf], 0.05)
    q95_infections <- quantile(no_inf[ , tot_inf], 0.95)

    # get number of iterations
    iterations <- max(results[ , sim_no])

    # calculate percent of simulations which died out
    die_out <- sum(no_inf[ , die_out == T]) / iterations * 100

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
  } else {
    scenario_results <- no_inf
  }
  return(scenario_results)
}
