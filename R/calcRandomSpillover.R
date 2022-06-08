calcRandomSpillover <- function(clinical_state_vector,
                                move_restricted_off,
                                move_restricted_on,
                                trans.type) {

  spread.onSite.Index <- site.index[!clinical_state_vector & !move_restricted_on]
  spread.offSite.Index <- site.index[clinical_state_vector & !move_restricted_off]
  Fomite_Transmission_Independant_Prob <- 1 / ListRunTimeParameters[[trans.type]]
  noInfectedSites <- length(spread.offSite.Index)
  noSusceptibleSites <- length(spread.onSite.Index)

  if (noSusceptibleSites != 0) {

    site <- sample.int(noSusceptibleSites, size = 1, replace = TRUE)

    listInfectionRates.objects <- list(rep.int(trans.type, times = noInfectedSites),
                                      rep.int(spread.onSite.Index[site], times = noInfectedSites),
                                      rep.int(Fomite_Transmission_Independant_Prob, times = noInfectedSites),
                                      rep.int(NA, times = noInfectedSites),
                                      noInfectedSites)

  } else {

    listInfectionRates.objects <- list(NULL, NULL, NULL, NULL)
  }

  return(listInfectionRates.objects)
}
