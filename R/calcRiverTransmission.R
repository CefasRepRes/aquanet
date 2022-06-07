calcRiverTransmission <- function(matrix_river_distances_prob,
                                  clinical_state_vector,
                                  move_restricted_off,
                                  move_restricted_on,
                                  trans_type) {

  # multiply probability matrix by clinical sites where offsite movements are not restricted
  matrix_river_distances_prob <- matrix_river_distances_prob * (clinical_state_vector * !move_restricted_off)

  # transpose and multiply probability matrix by sites where onsite movements are not restricted
  matrix_river_distances_prob <- t(matrix_river_distances_prob) * !move_restricted_on

  # re-transpose probability matrix
  matrix_river_distances_prob <- t(matrix_river_distances_prob)

  # calculate infection rate for river transmission
  list_river_infection_rates <- aquanet::listRatesSusceptibleRiskContacts(spmatrix_risk_contacts = matrix_river_distances_prob,
                                                                          state_vector = clinical_state_vector,
                                                                          trans_type = trans_type)

  return(list_river_infection_rates)
}
