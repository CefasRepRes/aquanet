calcRandomSpillover <- function(clinical_state_vector,
                                spread_restricted_off,
                                spread_restricted_on,
                                site_indices,
                                trans_type,
                                run_time_params) {
  # create vector of susceptible site IDs with no restricted spread on site
  spread.onSite.Index <- site_indices[!clinical_state_vector & !spread_restricted_on]
  noSusceptibleSites <- length(spread.onSite.Index)

  # create vector of infected site IDs with no resticted spread off site
  spread.offSite.Index <- site_indices[clinical_state_vector & !spread_restricted_off]
  noInfectedSites <- length(spread.offSite.Index)

  # calculate tranmission probability
  Fomite_Transmission_Independant_Prob <- 1 / run_time_params[[trans_type]]

  # if there are susceptible sites:
  if (noSusceptibleSites != 0) {
    # take a sample of one susceptible site
    site <- sample.int(noSusceptibleSites, size = 1, replace = TRUE)

    # determine which column number the trans_type string refers to
    trans_num <- which(colnames(run_time_params) == trans_type)

    # create populated output list
    listInfectionRates.objects <- list(rep.int(trans_num, times = noInfectedSites),
                                       rep.int(spread.onSite.Index[site], times = noInfectedSites),
                                       rep.int(Fomite_Transmission_Independant_Prob, times = noInfectedSites),
                                       rep.int(NA, times = noInfectedSites),
                                       noInfectedSites)

  # if there are no susceptible sites:
  } else {
    # create empty output list
    listInfectionRates.objects <- list(NULL, NULL, NULL, NULL)
  }

  return(listInfectionRates.objects)
}
