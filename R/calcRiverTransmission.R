calcRiverTransmission <- function(distanceMatrix, state_vector, spread.offSite.prevented, spread.onSite.prevented, trans_type) {
  distanceMatrix <- distanceMatrix * (state_vector * !spread.offSite.prevented)
  distanceMatrix <- t(distanceMatrix) * !spread.onSite.prevented
  distanceMatrix <- t(distanceMatrix)

  listInfectionRates.objects <- aquanet::listRatesSusceptibleRiskContacts(spmatrix_risk_contacts = distanceMatrix,
                                                                          state_vector = state_vector,
                                                                          trans_type = trans_type)

  return(listInfectionRates.objects)
}
