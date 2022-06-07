calcRiverTransmission <- function(matrix_river_distances_prob,
                                  clinical_state_vector,
                                  move_restricted_off,
                                  move_restricted_on,
                                  trans_type) {

  matrix_river_distances_prob <- matrix_river_distances_prob * (clinical_state_vector * !move_restricted_off)
  matrix_river_distances_prob <- t(matrix_river_distances_prob) * !move_restricted_on
  matrix_river_distances_prob <- t(matrix_river_distances_prob)

  list_river_infection_rates <- aquanet::listRatesSusceptibleRiskContacts(spmatrix_risk_contacts = matrix_river_distances_prob,
                                                                          state_vector = clinical_state_vector,
                                                                          trans_type = trans_type)

  return(list_river_infection_rates)
}
