update_rate <- function(state_vector,
                        control_matrix,
                        farm_vector,
                        withinCatchmentMovements.objects,
                        matrix_movements_prob,
                        run_time_params,
                        winter) {

  # create empty list for transition rate storage
  trans_rates <- vector(mode = "list", length = 4)


  ### define site types ----

  # create vector of infected farms that are NOT latent or fallow (leading to recovery)
  farms_I <- state_vector * sites_I_recovery * farm_vector

  # sites that are not fallow, not allowed to import fish and not latent
  sites_I_recovery <- !as.logical(control_matrix[ , c(4, 5, 6)] %*% rep(1, 3))

  # create vector of infected fisheries that are NOT latent (leading to latency)
  fisheries_I <- state_vector * sites_I_recovery * !farm_vector

  # create a vector of latently infected sites
  sites_L <- as.logical(control_matrix[ , 6])

  # create vector of sites (farms) that are fallow and infected (fallow/ready to restock post fallow)
  farms_fallow <- state_vector * (control_matrix[ , 4] + control_matrix[ , 5])

  # create vector of sites which have been contact traced
  sites_contact_traced <- control_matrix[ , 7]

  # create vector of sites that can be controlled (infection present and not detected)
  sites_I_undetected <- control_matrix[ , 1]

  # create vector of sites (farms) that can become fallow
  farms_I_controlled <- movement.restrictions.allSite * culling_vector

  # create vector of clinically infected sites
  clinical.vector <- state_vector * !control_matrix[ , 6]





  # When a site has been contact traced it will be subject to movement controls, but
  # will not be culled, released from controls or used to infer which catchments
  # need to be controlled until infection has been confirmed.
  # Hence contact tracing isn't used to calculate 'movement.restrictions.bySite'

  ### define movement restrictions ----
  # Scenario 1: culling - farms in the surveillance and fallow periods (infection detected, movements restricted and fallow)
  # Note: for culling all sites transition from 2 -> 4
  movement.restrictions.bySite <- as.logical(control_matrix[ , c(2, 3, 4, 5)] %*% rep(1, 4))

  # Scenario 2: surveillance - sites in the surveillance period
  movement.restrictions.allSite <- as.logical(control_matrix[ , c(2, 3)] %*% rep(1, 2))

  ####### Include sites in surveillance stage 3 as they are allowed to move fish within infected catchments
  transport.onSite.prevented <- as.logical(control_matrix[ , c(2, 4, 5, 6, 7)] %*% rep(1, 5))
  transport.offSite.prevented <- as.logical(control_matrix[ , c(2, 4, 5, 7)] %*% rep(1, 4))

  # sites that are fallow, allowed to import fish or latent
  spread.onSite.prevented <- as.logical(control_matrix[ , c(4, 5, 6)] %*% rep(1, 3))
  spread.offSite.prevented <- spread.onSite.prevented




  # Identify contacts originating from infected sites,
  # excluding contacts from sites that can not transport off site
  atriskcontacts <- matrix_movements_prob * (state_vector * !transport.offSite.prevented)

  # Identify, and remove contacts ending at controlled sites,
  # excluding contacts from sites that can not receive transported stuff
  atriskcontacts <- Matrix::t(atriskcontacts) * !transport.onSite.prevented
  atriskcontacts <- Matrix::t(atriskcontacts)

  withinCatchmentMovements.out.objects <- aquanet::excludeWithinCatchmentMovements(move_restricted_sites = movement.restrictions.bySite,
                                                                                   spmatrix_risk_contacts = atriskcontacts,
                                                                                   catchment_movements = withinCatchmentMovements.objects,
                                                                                   matrix_movements_prob = matrix_movements_prob)
  atriskcontacts <- withinCatchmentMovements.out.objects[[1]]
  withinCatchmentMovements.objects <- withinCatchmentMovements.out.objects[[2]]

  # Create an edge list for live fish movements from infected to exposed sites
  susceptable.sites.exposure.rate.objects <- aquanet::listRatesSusceptibleRiskContacts(spmatrix_risk_contacts = atriskcontacts,
                                                                                       state_vector = state_vector,
                                                                                       trans_type = 0)
  trans_rates <- aquanet::combineTransitionRates(list_append = susceptable.sites.exposure.rate.objects,
                                                 list_base = trans_rates)


  ### calculate transition rates ----

  # Rate 1: farm transitions from infected to subclinical infection
  rate_farm_recovery <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                     state_vector = farms_I,
                                                     trans_type = "Site_Recovers",
                                                     site_indices = site.index,
                                                     infection_state = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_farm_recovery,
                                                 list_base = trans_rates)


  # Rate 2: fishery transitions from infected to subclinical infection
  rate_fishery_latency <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = fisheries_I,
                                                       trans_type = "Infection_Becomes_Subclinical",
                                                       site_indices = site.index,
                                                       infection_state = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_fishery_latency,
                                                 list_base = trans_rates)


  # Rate 3: transition from subclinical infection (farms and fisheries)
  rate_site_cleared <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                    state_vector = sites_L,
                                                    trans_type = "Clearing_Of_Latency_From_Infected_Sites",
                                                    site_indices = site.index,
                                                    infection_state = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_cleared,
                                                 list_base = trans_rates)


  # Rate 4: rate at which fallow sites are disinfected
  rate_farm_disinfected <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                        state_vector = farms_fallow,
                                                        trans_type = "Reinfection_After_Restocking_Const",
                                                        site_indices = site.index,
                                                        infection_state = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_farm_disinfected,
                                                 list_base = trans_rates)

  # Rate 5: rate at which contact traced sites will be tested
  rate_sites_ct_tested <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = sites_contact_traced,
                                                       trans_type = "Contact_Detection",
                                                       site_indices = site.index,
                                                       infection_state = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_sites_ct_tested,
                                                 list_base = trans_rates)


  # Rate 6: rate of detection in infected but undetected sites
  rate_site_detected <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                     state_vector = sites_I_undetected,
                                                     trans_type = "Detection_Reporting_Disease",
                                                     site_indices = site.index,
                                                     infection_state = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_detected,
                                                 list_base = trans_rates)


  # Rate X: rate at which sites become fallow
  rate_farm_fallow <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                                       state_vector = farms_I_controlled,
                                                                       trans_type = "Time_Required_Cull_Site",
                                                                       site_indices = site.index,
                                                                       infection_state = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_farm_fallow,
                                                 list_base = trans_rates)


  ## if inside active transmission period get rates of transmission for mechanisms other than LFM: -----

  if (winter == FALSE) {
    # Rate 7: rate at which sites revert from latent to clinical infection
    sites_L_recrudesce <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = sites_L,
                                                       trans_type = "Second_Outbreak_Due_To_Subclinical_Infection",
                                                       site_indices = site.index,
                                                       infection_state = 1)
    trans_rates <- aquanet::combineTransitionRates(list_append = sites_L_recrudesce, list_base = trans_rates)


    # Rate 8: probability of a contact occurring downstream of an outbreak via the river network
    graph.riverDownstream.objects <- graph.riverDistance.objects[[1]]
    riverDownstream.matrix <- graph.riverDownstream.objects[[2]]
    susceptable.sites.exposure.byRiver.downstream.objects <- aquanet::calcRiverTransmission(matrix_river_distances_prob = riverDownstream.matrix,
                                                                                            clinical_state_vector = clinical.vector,
                                                                                            spread_restricted_off = spread.offSite.prevented,
                                                                                            spread_restricted_on = spread.onSite.prevented,
                                                                                            trans_type = 10)
    trans_rates <- aquanet::combineTransitionRates(list_append = susceptable.sites.exposure.byRiver.downstream.objects,
                                                   list_base = trans_rates)


    # Rate 9: probability of a contact occurring due to local fomite transmission
    fomite.matrix <- graph.estimateSiteDistances.objects[[2]]
    susceptable.sites.exposure.byFomites.objects <- aquanet::calcRiverTransmission(matrix_river_distances_prob = fomite.matrix,
                                                                                   clinical_state_vector = clinical.vector,
                                                                                   spread_restricted_off = spread.offSite.prevented,
                                                                                   spread_restricted_on = spread.onSite.prevented,
                                                                                   trans_type = 14)
    trans_rates <- aquanet::combineTransitionRates(list_append = susceptable.sites.exposure.byFomites.objects,
                                                   list_base = trans_rates)



    ## if there are susceptible sites without restrictions preventing spread on site: ----

    if (sum(!state_vector & !spread.onSite.prevented) != 0) {

      # Rate 10: identify transitions from infected to susceptible sites that could occur randomly regardless of mechanism
      # Note: excludes contacts from sites whose restrictions prevent this mechanism of transmission
      sites_random_change <- aquanet::calcRandomSpillover(clinical_state_vector = clinical.vector,
                                                         spread_restricted_off = spread.offSite.prevented,
                                                         spread_restricted_on = spread.onSite.prevented,
                                                         site_indices = site.index,
                                                         trans_type = "Fomite_Transmission_Independant_Prob",
                                                         run_time_params = run_time_params)
      trans_rates <- aquanet::combineTransitionRates(list_append = sites_random_change,
                                                     list_base = trans_rates)
    }
  }

  return(list(trans_rates, withinCatchmentMovements.objects, movement.restrictions.bySite))
}
