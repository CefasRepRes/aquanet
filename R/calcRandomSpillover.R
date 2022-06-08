calcRandomSpillover <- function(clinical_state_vector,
                                spread_restricted_off,
                                spread_restricted_on,
                                site_indices,
                                trans_type,
                                run_time_params) {
  # create vector of susceptible site IDs with no restricted spread on site
  spread.onSite.Index <- site_indices[!clinical_state_vector & !spread_restricted_on]
  n_sites_S <- length(spread.onSite.Index)

  # create vector of infected site IDs with no resticted spread off site
  spread.offSite.Index <- site_indices[clinical_state_vector & !spread_restricted_off]
  n_sites_I <- length(spread.offSite.Index)

  # calculate tranmission probability
  Fomite_Transmission_Independant_Prob <- 1 / run_time_params[[trans_type]]

  # if there are susceptible sites:
  if (n_sites_S != 0) {
    # take a sample of one susceptible site
    site <- sample.int(n_sites_S, size = 1, replace = TRUE)

    # determine which column number the trans_type string refers to
    trans_num <- which(colnames(run_time_params) == trans_type)

    # create populated output list
    listInfectionRates.objects <- list(rep.int(trans_num, times = n_sites_I),
                                       rep.int(spread.onSite.Index[site], times = n_sites_I),
                                       rep.int(Fomite_Transmission_Independant_Prob, times = n_sites_I),
                                       rep.int(NA, times = n_sites_I),
                                       n_sites_I)

  # if there are no susceptible sites:
  } else {
    # create empty output list
    listInfectionRates.objects <- list(NULL, NULL, NULL, NULL)
  }

  return(listInfectionRates.objects)
}
