listTransitionRates <- function(run_time_params, state_vector, trans_type, site_indices,
                                infection_state) {
  # get column number for trans_type in run_time_params
  trans_num <- which(colnames(run_time_params) == trans_type)

  # get probability from input parameter file
  prob <- run_time_params[[trans_type]]

  # create logical vector of sites in input state specified
  state_logical <- state_vector == infection_state

  # get the position and total number of sites in input state specified
  position <- site_indices[state_logical]
  n_rates <- length(position)

  # create vector of transition rates, transition rate type, and infection source
  rate <- rep(1 / prob, times = n_rates)
  rate_type <- rep(trans_num, times = n_rates)
  source_site <- rep(NA, times = n_rates)

  return(list(rate_type, position, rate, source_site, n_rates))
}
