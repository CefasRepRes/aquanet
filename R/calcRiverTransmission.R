calcRiverTransmission <- function(distanceMatrix, clinical_state_vector, move_restricted_off, move_restricted_on, trans_type) {
  distanceMatrix <- distanceMatrix * (clinical_state_vector * !move_restricted_off)
  distanceMatrix <- t(distanceMatrix) * !move_restricted_on
  distanceMatrix <- t(distanceMatrix)

  listInfectionRates.objects <- aquanet::listRatesSusceptibleRiskContacts(spmatrix_risk_contacts = distanceMatrix,
                                                                          state_vector = clinical_state_vector,
                                                                          trans_type = trans_type)

  return(listInfectionRates.objects)
}
