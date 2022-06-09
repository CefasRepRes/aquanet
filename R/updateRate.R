update_rate <- function(state_vector,
                        control_matrix,
                        farm_vector,
                        withinCatchmentMovements.objects,
                        matrix_movements_prob,
                        run_time_params) {

  # create empty list for transition rate storage
  trans_rates <- vector(mode = "list", length = 4)

  # create vector of clinically infected sites
  clinical.vector <- state_vector * !control_matrix[ , 6]

  # When a site has been contact traced it will be subject to movement controls, but
  # will not be culled, released from controls or used to infer which catchments
  # need to be controlled until infection has been confirmed.
  # Hence contact tracing isn't used to calculate 'movement.restrictions.bySite'

  ######## sites in the surveillance and fallow periods
  movement.restrictions.bySite <- as.logical(control_matrix[ , c(2, 3, 4, 5)] %*% rep(1, 4))

  ####### sites in the surveillance period - different for culling and surveillance scenario as all sites in culling scenario transition from 2 to 4
  movement.restrictions.allSite <- as.logical(control_matrix[ , c(2, 3)] %*% rep(1, 2))

  ####### Include sites in surveillance stage 3 as they are allowed to move fish within infected catchments
  transport.onSite.prevented <- as.logical(control_matrix[ , c(2, 4, 5, 6, 7)] %*% rep(1, 5))
  transport.offSite.prevented <- as.logical(control_matrix[ , c(2, 4, 5, 7)] %*% rep(1, 4))

  # sites that are not fallow, not allowed to import fish and not latent
  sites_I_recovery <- !as.logical(control_matrix[ , c(4, 5, 6)] %*% rep(1, 3))

  # sites that are fallow, allowed to import fish or latent
  spread.onSite.prevented <- as.logical(control_matrix[ , c(4, 5, 6)] %*% rep(1, 3))
  spread.offSite.prevented <- spread.onSite.prevented

  # Identify contacts originating from infected sites,
  # excluding contacts from sites that can not transport off site
  atriskcontacts <- matrix_movements_prob * (state_vector * !transport.offSite.prevented)

  # Identify, and remove contacts ending at controlled sites,
  # excluding contacts from sites that can not receive transported stuff
  atriskcontacts <- t(atriskcontacts) * !transport.onSite.prevented
  atriskcontacts <- t(atriskcontacts)

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


  ### calculate transition rates ----------------

  # Rate 1: farm transitions from infected to subclinical infection
  # create vector of infected farms that are NOT latent or fallow (leading to recovery)
  farms_I <- state_vector * sites_I_recovery * farm_vector
  rate_farm_recovery <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                     state_vector = farms_I,
                                                     trans_type = "Site_Recovers",
                                                     site_indices = site.index,
                                                     infection_status = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_farm_recovery,
                                                 list_base = trans_rates)


  # Rate 2: fishery transitions from infected to subclinical infection
  # create vector of infected fisheries that are NOT latent (leading to latency)
  fisheries_I <- state_vector * sites_I_recovery * !farm_vector
  rate_fishery_latency <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = fisheries_I,
                                                       trans_type = "Infection_Becomes_Subclinical",
                                                       site_indices = site.index,
                                                       infection_status = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_fishery_latency,
                                                 list_base = trans_rates)


  # Rate 3: transition from subclinical infection (farms and fisheries)
  # create a vector of latently infected sites
  sites_L <- as.logical(control_matrix[ , 6])
  rate_site_cleared <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                    state_vector = sites_L,
                                                    trans_type = "Clearing_Of_Latency_From_Infected_Sites",
                                                    site_indices = site.index,
                                                    infection_status = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_cleared,
                                                 list_base = trans_rates)


  # Rate 4: rate at which fallow sites are disinfected
  # create vector of sites (farms) that are fallow and infected (fallow/ready to restock post fallow)
  farms_fallow <- state_vector * (control_matrix[ , 4] + control_matrix[ , 5])
  rate_farm_disinfected <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                        state_vector = farms_fallow,
                                                        trans_type = "Reinfection_After_Restocking_Const",
                                                        site_indices = site.index,
                                                        infection_status = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_farm_disinfected,
                                                 list_base = trans_rates)

  # Rate 5: rate at which contact traced sites will be tested
  # create vector of sites which have been contact traced
  sites_contact_traced <- control_matrix[ , 7]
  rate_sites_ct_tested <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = sites_contact_traced,
                                                       trans_type = "Contact_Detection",
                                                       site_indices = site.index,
                                                       infection_status = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_sites_ct_tested,
                                                 list_base = trans_rates)

  ########
  # Identify sites that are under surveillance or management
  # This is so the simulation continues even if there is no infection left in the system
  # The simualiton will check roughly every 42 days to see if the management status' need updating
  # Create a vector showing the position of sites that are under surveillance
  # Create a vector showing the rates at which the sites under surveillance should be allowed to trade again
  #surveillance.sites <- as.logical(control_matrix[ , c(2, 3, 4, 5)] %*% rep(1, 4))
  #surveillance.sites <- ifelse(surveillance.sites > 0, 1, 0)
  # surveillance.sites.rate <- aquanet::listTransitionRates(run_time_params = run_time_params,
  #                                                         state_vector = surveillance.sites,
  #                                                         trans_type = "Early_Controls_Fisheries",
  #                                                         site_indices = site.index,
  #                                                         infection_status = 1)
  #trans_rates <- aquanet::combineTransitionRates(list_append = surveillance.sites.rate, list_base = trans_rates)
  ########

  # Identify sites that can become controlled
  # Create a vector showing the position of sites that can be controlled
  # Create a vector with the control rate of infected sites
  infected.sites.notControlled <- control_matrix[ , 1]
  infected.sites.control.rate.objects <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                                      state_vector = infected.sites.notControlled,
                                                                      trans_type = "Detection_Reporting_Disease",
                                                                      site_indices = site.index,
                                                                      infection_status = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = infected.sites.control.rate.objects, list_base = trans_rates)

  if (winter == FALSE) {
    # Identify any latent, infected sites
    # Create a vector showing the position of latent sites
    # Create a vector with the rate at which sites revert to active expression of disease
    latent.sites.secondOutbreak <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                                state_vector = sites_L,
                                                                trans_type = "Second_Outbreak_Due_To_Subclinical_Infection",
                                                                site_indices = site.index,
                                                                infection_status = 1)
    trans_rates <- aquanet::combineTransitionRates(list_append = latent.sites.secondOutbreak, list_base = trans_rates)


    # Calculate the probability of a contact occuring downstream of an outbreak, through the river network
    graph.riverDownstream.objects <- graph.riverDistance.objects[[1]]
    riverDownstream.matrix <- graph.riverDownstream.objects[[2]]
    susceptable.sites.exposure.byRiver.downstream.objects <- aquanet::calcRiverTransmission(riverDownstream.matrix, clinical.vector, spread.offSite.prevented, spread.onSite.prevented, 10)
    trans_rates <- aquanet::combineTransitionRates(list_append = susceptable.sites.exposure.byRiver.downstream.objects,
                                                   list_base = trans_rates)

    ########
    ######## Calculate the probability of a contact occuring due to local fomite transmission
    fomite.matrix <- graph.estimateSiteDistances.objects[[2]]
    susceptable.sites.exposure.byFomites.objects <- aquanet::calcRiverTransmission(fomite.matrix, clinical.vector, spread.offSite.prevented, spread.onSite.prevented, 14)
    trans_rates <- aquanet::combineTransitionRates(list_append = susceptable.sites.exposure.byFomites.objects,
                                                   list_base = trans_rates)
    ########


    #Identify potential transitions from infected to susceptable sites that could occur randomly, regardless of the proposed mechanism
    #Exclude contacts from sites that can not perticipate in such a mechanism of transmition
    if (sum(!state_vector & !spread.onSite.prevented) != 0) {
      spill.over.objects <- calcRandomSpillover(clinical.vector, spread.offSite.prevented, spread.onSite.prevented, 11)
      trans_rates <- aquanet::combineTransitionRates(list_append = spill.over.objects,
                                                     list_base = trans_rates)
    }
  }

  # Identify sites that may become fallow
  # Create a vector showing the position of sites that can become fallow
  # Create a vector with the rate at which sites become fallow
  controlled.farms <- movement.restrictions.allSite * culling_vector
  controlled.sites.fallow.rate.objects <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                                       state_vector = controlled.farms,
                                                                       trans_type = "Time_Required_Cull_Site",
                                                                       site_indices = site.index,
                                                                       infection_status = 1)
  trans_rates <- aquanet::combineTransitionRates(list_append = controlled.sites.fallow.rate.objects,
                                                 list_base = trans_rates)

  return(list(trans_rates, withinCatchmentMovements.objects, movement.restrictions.bySite))
}
