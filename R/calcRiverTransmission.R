calcRiverTransmission = function(distanceMatrix, state_vector, spread.offSite.prevented, spread.onSite.prevented, trans.type) {
  distanceMatrix = distanceMatrix * (state_vector * !spread.offSite.prevented)
  distanceMatrix = t(distanceMatrix) * !spread.onSite.prevented
  distanceMatrix = t(distanceMatrix)

  listInfectionRates.objects = listInfectionRates(distanceMatrix, state_vector, trans.type)

  return(listInfectionRates.objects)
}
