listInfectionRates <- function(spmatrix_risk_contacts, state_vector, trans_type) {
  # convert 'dgCMatrix' to 'dgTMatrix'
  spmatrix_risk_contacts <- as(t(spmatrix_risk_contacts), "dgTMatrix")

  # get matrix value coordinates (i, j) and values (x)
  # NOTE: 0-based coordinates (most R objects have 1-based coordinates)
  slots <- c("i", "j", "x")
  slots_risk_contacts <- lapply(setNames(slots, slots), FUN = function(x) { slot(spmatrix_risk_contacts, x) })

  # create a logical vector stating whether x is non-zero
  non_zero <- slots_risk_contacts[["x"]] != 0

  # extract coordinates (i, j) and values (x) for at risk contact probabilities which are non-zero
  slots_risk_contacts <- lapply(setNames(slots, slots), FUN = function(x) { slots_risk_contacts[[x]][non_zero]})

  # create logical vector of edges with potential to infect susceptible sites NOTE: i + 1 as 0-based coordinates
  edge_susceptible <- state_vector[slots_risk_contacts[["i"]] + 1] == 0

  # subset risk contacts leading to susceptible sites
  contacts <- lapply(setNames(slots, c("position", "source", "rate")),
                     FUN = function(x) { slots_risk_contacts[[x]][edge_susceptible]})

  # determine number of contacts
  n_contacts <- length(contacts[["rate"]])
  contacts_rate_type <- rep.int(0, times = n_contacts)

  return(list(contacts_rate_type, contacts[["position"]], contacts[["rate"]], contacts[["source"]], n_contacts))
}
