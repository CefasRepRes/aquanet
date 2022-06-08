calcRandomSpillover <- function(clinical_state_vector,
                                spread_restricted_off,
                                spread_restricted_on,
                                site_indices,
                                trans_type,
                                run_time_params) {

  spread.onSite.Index <- site_indices[!clinical_state_vector & !spread_restricted_on]
  spread.offSite.Index <- site_indices[clinical_state_vector & !spread_restricted_off]
  Fomite_Transmission_Independant_Prob <- 1 / run_time_params[[trans_type]]
  noInfectedSites <- length(spread.offSite.Index)
  noSusceptibleSites <- length(spread.onSite.Index)

  if (noSusceptibleSites != 0) {

    site <- sample.int(noSusceptibleSites, size = 1, replace = TRUE)

    trans_num <- which(colnames(run_time_params) == trans_type)

    listInfectionRates.objects <- list(rep.int(trans_num, times = noInfectedSites),
                                      rep.int(spread.onSite.Index[site], times = noInfectedSites),
                                      rep.int(Fomite_Transmission_Independant_Prob, times = noInfectedSites),
                                      rep.int(NA, times = noInfectedSites),
                                      noInfectedSites)

  } else {

    listInfectionRates.objects <- list(NULL, NULL, NULL, NULL)
  }

  return(listInfectionRates.objects)
}
