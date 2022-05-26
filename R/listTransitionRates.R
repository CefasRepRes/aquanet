listTransitionRates <- function(run_time_params,
                                state_vector, trans.type, site.index, state.match) {
  # get probability from input parameter file
  prob <- run_time_params[[trans.type]]

  # create logical vector of sites in input state specified
  state.logical <- state_vector == state.match

  # get the position and total number of sites in input state specified
  position <- site.index[state.logical]
  state.no <- length(position)

  # create vector of transition rates, transition rate type, and infection source
  state.rate <- rep(1 / prob, times = state.no)
  rate_type <- rep(trans.type, times = state.no)
  source.infection <- rep(NA, times = state.no)

  return(list(rate_type, position, state.rate, source.infection, state.no))
}
