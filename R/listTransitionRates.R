listTransitionRates = function(state_vector, trans.type, site.index, state.match) {


  prob = ListRunTimeParameters[[trans.type]]

  ## Reduce detection time when the number of cumulative infected sites gets above 10
  #if(sum(cumulativeState_vector) > 10 & trans.type == 6){
  #  prob = 14
  #}
  #else {
  #  prob = ListRunTimeParameters[[trans.type]]
  #}

  state.logical = state_vector == state.match
  state.pos = site.index[state.logical]
  state.no = length(state.pos)
  state.rate = rep(1 / prob, times = state.no)
  state.rate.type = rep(trans.type, times = state.no)
  source.infection = rep(NA, times = state.no)

  return(list(state.rate.type, state.pos, state.rate, source.infection, state.no))
}
