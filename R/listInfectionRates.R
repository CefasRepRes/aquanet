listInfectionRates = function(distanceMatrix, state_vector, trans.type) {
  # Convert matrix to 'dgTMatrix'
  distanceMatrix = as(t(distanceMatrix), 'dgTMatrix')

  # 'i' and 'j' vectors are coordinates, which are 0-based coordinates (most R objects have 1-based coordinates)
  # Extract each value's coordinates, and save them into seperate objects
  i = distanceMatrix@i
  j = distanceMatrix@j
  x = distanceMatrix@x

  # Create a logical vector testing whether x is non-zero
  nonEmpty.logical = x != 0

  # Extract values and the associated coordinates for objects which are non-zero
  i = i[nonEmpty.logical]
  j = j[nonEmpty.logical]
  x = x[nonEmpty.logical]

  # Extract edges which have potential to infect susceptable sites
  edges.potential.infect.logical = state_vector[i + 1] == 0

  state.pos = i[edges.potential.infect.logical]
  state.rate = x[edges.potential.infect.logical]
  state.no = length(state.rate)
  state.rate.type = rep.int(trans.type, times = state.no)
  source.infection = j[edges.potential.infect.logical]

  return(list(state.rate.type, state.pos, state.rate, source.infection, state.no))
}
