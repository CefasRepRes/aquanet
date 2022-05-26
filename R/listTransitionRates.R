listTransitionRates <- function(run_time_params,
                                state_vector, trans.type, site.index, state.match) {
  # get probability from input parameter file
  prob <- run_time_params[[trans.type]]

  # create logical vector of sites in input state specified
  state.logical <- state_vector == state.match

  # get the position and total number of sites in input state specified
  state.pos <- site.index[state.logical]
  state.no <- length(state.pos)

  # create vector of transition rates, transition rate type, and infection source
  state.rate <- rep(1 / prob, times = state.no)
  state.rate.type <- rep(trans.type, times = state.no)
  source.infection <- rep(NA, times = state.no)

  return(list(state.rate.type, state.pos, state.rate, source.infection, state.no))
}
