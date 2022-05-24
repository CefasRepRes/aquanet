listInfectionRates <- function(spmatrix_risk_contacts, state_vector, trans.type) {
  # convert 'dgCMatrix' to 'dgTMatrix'
  spmatrix_risk_contacts <- as(t(spmatrix_risk_contacts), "dgTMatrix")

  # get matrix value coordinates (i, j) and values (x)
  # NOTE: 0-based coordinates (most R objects have 1-based coordinates)
  i <- spmatrix_risk_contacts@i
  j <- spmatrix_risk_contacts@j
  x <- spmatrix_risk_contacts@x

  # create a logical vector stating whether x is non-zero
  nonEmpty.logical <- x != 0

  # extract coordinates (i, j) and values (x) for at risk contact probabilities which are non-zero
  i <- i[nonEmpty.logical]
  j <- j[nonEmpty.logical]
  x <- x[nonEmpty.logical]

  # create logical vector of edges with potential to infect susceptible sites NOTE: i + 1 as 0-based coordinates
  edges.potential.infect.logical <- state_vector[i + 1] == 0

  # subset risk contacts leading to susceptible sites
  state.pos <- i[edges.potential.infect.logical]
  source.infection <- j[edges.potential.infect.logical]
  state.rate <- x[edges.potential.infect.logical]

  # determine number of contacts
  state.no <- length(state.rate)
  state.rate.type <- rep.int(trans.type, times = state.no)

  return(list(state.rate.type, state.pos, state.rate, source.infection, state.no))
}
