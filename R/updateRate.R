update_rate <- function(state_vector,
                        site_indices,
                        control_matrix,
                        farm_vector,
                        culling_vector,
                        withinCatchmentMovements.objects,
                        matrix_movements_prob,
                        run_time_params,
                        winter) {

  ### define movement restrictions ----

  # Scenario 1: culling - all sites in the surveillance and fallow periods
  # Note: for culling all sites transition from 2 -> 4
  # Note: when sites are contact traced, movements are controlled, but not culled until infection is confirmed
  # therefore contact tracing is not used to determine these movement restrictions
  sites_all_movement_restricted <- as.logical(control_matrix[ , c(2, 3, 4, 5)] %*% rep(1, 4))

  # Scenario 2: surveillance - sites in the surveillance period
  sites_movement_restricted <- as.logical(control_matrix[ , c(2, 3)] %*% rep(1, 2))

  # sites where movement is restricted on or off site (Note: latent sites can import but not export)
  # Note: include sites in surveillance stage 3 as they can move fish within infected catchments
  transport_prevented_on <- as.logical(control_matrix[ , c(2, 4, 5, 6, 7)] %*% rep(1, 5))
  transport_prevented_off <- as.logical(control_matrix[ , c(2, 4, 5, 7)] %*% rep(1, 4))

  # sites that have fallowed, are empty and can restock or are latently infected - i.e. cannot spread
  spread_prevented_on <- as.logical(control_matrix[ , c(4, 5, 6)] %*% rep(1, 3))
  spread_prevented_off <- spread_prevented_on


  ### define site types ----

  # sites that are NOT fallow, post-fallow state, or latent (i.e. not recovering)
  sites_I_recovery <- !as.logical(control_matrix[ , c(4, 5, 6)] %*% rep(1, 3))

  # create vector of infected farms that are NOT latent or fallow state (leading to recovery)
  farms_I <- state_vector * sites_I_recovery * farm_vector

  # create vector of infected fisheries that are NOT latent (or fallow state) (leading to latency)
  fisheries_I <- state_vector * sites_I_recovery * !farm_vector

  # create a vector of latently infected sites
  sites_L <- as.logical(control_matrix[ , 6])

  # create vector of sites (farms) that are infected and in the fallow or post-fallow state
  farms_fallow <- state_vector * (control_matrix[ , 4] + control_matrix[ , 5])

  # create vector of sites which have been contact traced (are in infected catchment)
  sites_contact_traced <- control_matrix[ , 7]

  # create vector of sites that could be controlled (infection present and not detected)
  sites_I_undetected <- control_matrix[ , 1]

  # create vector of sites that can become fallow as they can be culled
  farms_I_controlled <- sites_movement_restricted * culling_vector

  # create vector of clinically infected sites
  clinical.vector <- state_vector * !control_matrix[ , 6]


  ### identify LFM contacts carrying risk ----

  # retain contact probabilities where origin site is infected with unrestricted transport off site
  matrix_risk_contacts <- matrix_movements_prob * (state_vector * !transport_prevented_off)

  # retain contact probabilities where receiving sites have no restricted transport on site
  matrix_risk_contacts <- Matrix::t(matrix_risk_contacts) * !transport_prevented_on
  matrix_risk_contacts <- Matrix::t(matrix_risk_contacts)

  # exclude within catchment movements
  risk_contacts_catch_corrected <- aquanet::excludeWithinCatchmentMovements(move_restricted_sites = sites_all_movement_restricted,
                                                                            spmatrix_risk_contacts = matrix_risk_contacts,
                                                                            catchment_movements = withinCatchmentMovements.objects,
                                                                            matrix_movements_prob = matrix_movements_prob)


  ### calculate LFM infection rate ----

  # Infection Rate 1: exposure rate of LFM contacts from infected to susceptible sites
  rate_site_infection <- aquanet::listRatesSusceptibleRiskContacts(spmatrix_risk_contacts = risk_contacts_catch_corrected[[1]],
                                                                   state_vector = state_vector,
                                                                   trans_type = 0)


  ### calculate transition rates ----

  # Rate 1: farm transitions from infected to subclinical infection
  rate_farm_recovery <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                     state_vector = farms_I,
                                                     trans_type = "Site_Recovers",
                                                     site_indices = site_indices,
                                                     infection_state = 1)

  # Rate 2: fishery transitions from infected to subclinical infection
  rate_fishery_latency <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = fisheries_I,
                                                       trans_type = "Infection_Becomes_Subclinical",
                                                       site_indices = site_indices,
                                                       infection_state = 1)

  # Rate 3: transition from subclinical infection (farms and fisheries)
  rate_site_cleared <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                    state_vector = sites_L,
                                                    trans_type = "Clearing_Of_Latency_From_Infected_Sites",
                                                    site_indices = site_indices,
                                                    infection_state = 1)

  # Rate 4: rate at which fallow sites are disinfected
  rate_farm_disinfected <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                        state_vector = farms_fallow,
                                                        trans_type = "Reinfection_After_Restocking_Const",
                                                        site_indices = site_indices,
                                                        infection_state = 1)

  # Rate 5: rate at which contact traced sites will be tested
  rate_sites_ct_tested <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = sites_contact_traced,
                                                       trans_type = "Contact_Detection",
                                                       site_indices = site_indices,
                                                       infection_state = 1)

  # Rate 6: rate of detection in infected but undetected sites
  rate_site_detected <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                     state_vector = sites_I_undetected,
                                                     trans_type = "Detection_Reporting_Disease",
                                                     site_indices = site_indices,
                                                     infection_state = 1)

  # Rate X: rate at which sites become fallow
  rate_farm_fallow <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                                       state_vector = farms_I_controlled,
                                                                       trans_type = "Time_Required_Cull_Site",
                                                                       site_indices = site_indices,
                                                                       infection_state = 1)


  ### combine transition rates ----

  # create empty list for transition rate storage
  trans_rates <- vector(mode = "list", length = 4)

  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_infection, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_farm_recovery, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_fishery_latency, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_cleared, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_farm_disinfected, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_sites_ct_tested, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_detected, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_farm_fallow, list_base = trans_rates)


  ## if inside active transmission period get rates of transmission for mechanisms other than LFM: -----

  if (winter == FALSE) {
    # Rate 7: rate at which sites revert from latent to clinical infection
    sites_L_recrudesce <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = sites_L,
                                                       trans_type = "Second_Outbreak_Due_To_Subclinical_Infection",
                                                       site_indices = site_indices,
                                                       infection_state = 1)
    trans_rates <- aquanet::combineTransitionRates(list_append = sites_L_recrudesce, list_base = trans_rates)


    # Rate 8: probability of a contact occurring downstream of an outbreak via the river network
    graph.riverDownstream.objects <- graph.riverDistance.objects[[1]]
    riverDownstream.matrix <- graph.riverDownstream.objects[[2]]
    susceptable.sites.exposure.byRiver.downstream.objects <- aquanet::calcRiverTransmission(matrix_river_distances_prob = riverDownstream.matrix,
                                                                                            clinical_state_vector = clinical.vector,
                                                                                            spread_restricted_off = spread_prevented_off,
                                                                                            spread_restricted_on = spread_prevented_on,
                                                                                            trans_type = 10)
    trans_rates <- aquanet::combineTransitionRates(list_append = susceptable.sites.exposure.byRiver.downstream.objects,
                                                   list_base = trans_rates)


    # Rate 9: probability of a contact occurring due to local fomite transmission
    fomite.matrix <- graph.estimateSiteDistances.objects[[2]]
    susceptable.sites.exposure.byFomites.objects <- aquanet::calcRiverTransmission(matrix_river_distances_prob = fomite.matrix,
                                                                                   clinical_state_vector = clinical.vector,
                                                                                   spread_restricted_off = spread_prevented_off,
                                                                                   spread_restricted_on = spread_prevented_on,
                                                                                   trans_type = 14)
    trans_rates <- aquanet::combineTransitionRates(list_append = susceptable.sites.exposure.byFomites.objects,
                                                   list_base = trans_rates)



    ## if there are susceptible sites without restrictions preventing spread on site: ----

    if (sum(!state_vector & !spread_prevented_on) != 0) {

      # Rate 10: identify transitions from infected to susceptible sites that could occur randomly regardless of mechanism
      # Note: excludes contacts from sites whose restrictions prevent this mechanism of transmission
      sites_random_change <- aquanet::calcRandomSpillover(clinical_state_vector = clinical.vector,
                                                         spread_restricted_off = spread_prevented_off,
                                                         spread_restricted_on = spread_prevented_on,
                                                         site_indices = site_indices,
                                                         trans_type = "Fomite_Transmission_Independant_Prob",
                                                         run_time_params = run_time_params)
      trans_rates <- aquanet::combineTransitionRates(list_append = sites_random_change,
                                                     list_base = trans_rates)
    }
  }

  return(list(trans_rates, risk_contacts_catch_corrected[[2]], sites_all_movement_restricted))
}
