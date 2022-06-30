simulationCode = function(graph.contactp.objects, runs, tmax, batchNo, ListRunTimeParameters, graph.withinCatchmentEdges.objects, graph.catchment2Site.objects, graph.riverDistance.objects, graph.estimateSiteDistances.objects, farm_vector, associatedSiteControlType, locationSaveResults, initialNoInfections) {

  # Sparse Matrices and Data Tables are used for memory or computational efficiency
  # The packages are loaded here, since parallel execution does not allow them to be loaded earlier
  library(Matrix)
  library(data.table)

  # Record the number of steps in the simulation,
  # the number of operations in the simulation (including iterations where the model terminates prematurely)
  # the number of saved results, per a simulation
  noSteps = 0
  noOperations = 0
  numberFullSaves = 0

  # Retrieve the contact network, and the number of sites in the network
  contactp.length = graph.contactp.objects[[1]]
  contactp = graph.contactp.objects[[3]]
  contactp.siteNames = dimnames(contactp)[[1]]

  # Each site within the model has a unique position within a matrix
  # When necessary, this position can be expressed as a number, using the site index
  site.index = 0:(contactp.length - 1)

  # Matrix representing the site / catchment relationship
  graph.catchment2site.matrix2 = graph.catchment2Site.objects[[2]]

  # Matrix identifying which of the contacts occur between sites in the same catchment
  graph.withinCatchmentEdges.matrix = graph.withinCatchmentEdges.objects[[1]]

  # Number of catchments within the model0
  no.catchments = graph.catchment2site.matrix2@Dim[2]

  # Save the results every x number of iterations
  commitInterval = 5000
  iterationID.vector = 1:commitInterval

  # Number of different combinations of states possible within the model
  no.variables = 42

  # Create empty records, which are used to force enough memory to be allocated for results
  empty.vector = rep(0, contactp.length + no.variables)
  empty.vector.t = rep(0, 2)
  empty.vector.byState = rep(0, no.variables + 8)

  # Preallocate memory for storing results
  allStates.table = data.table(empty.vector)
  allStates.table[,as.character(iterationID.vector):=empty.vector]

  allStates.table.t = data.table(empty.vector.t)
  allStates.table.t[,as.character(iterationID.vector):=empty.vector.t]

  summaryStates.table = data.table(empty.vector.byState)
  summaryStates.table[,as.character(iterationID.vector):=empty.vector.byState]
  summaryStates.table[,c("empty.vector.byState"):=NULL]

  # Create a vector to record transition times, for diagnostic purposes
  record_transition_times = c()

  checkCatchmentLevelRestocking = function(control_matrix, tdiff) {
    # Extract a list of sites that are fallow, and those which are waiting to be restocked
    controlled.sites.c4.numeric = control_matrix[,4]
    controlled.sites.c5.numeric = control_matrix[,5]

    # List the number of sites that are fallow, and are waiting to be restocked, in each catchment
    catchments.no.c4.sites.present = as.vector(t(graph.catchment2site.matrix2) %*% controlled.sites.c4.numeric)
    catchments.no.c5.sites.present = as.vector(t(graph.catchment2site.matrix2) %*% controlled.sites.c5.numeric)

    # List catchments containing fallow sites, and those which are waiting to be restocked
    catchments.c4.sites.present.logical = catchments.no.c4.sites.present > 0
    catchments.c5.sites.present.logical = catchments.no.c5.sites.present > 0

    # Identify catchments that contain sites ready to be restocked, but no fallow sites
    catchments.all.sites.c5.status = rep(FALSE, no.catchments)
    catchments.all.sites.c5.status[catchments.c5.sites.present.logical] = catchments.no.c4.sites.present[catchments.c5.sites.present.logical] == 0

    # Identify catchments that contain fallow sites
    catchments.some.sites.c4.status = rep(FALSE, no.catchments)
    catchments.some.sites.c4.status[catchments.c4.sites.present.logical] = catchments.no.c4.sites.present[catchments.c4.sites.present.logical] != 0

    return(list(control_matrix, catchments.some.sites.c4.status, catchments.all.sites.c5.status))

  }

  combineTransitions = function(transition.objects, transition.rates) {
    # Type of transition
    transition.rates[[1]] = c(transition.rates[[1]],transition.objects[[1]])
    # Site subject to transition
    transition.rates[[2]] = c(transition.rates[[2]],transition.objects[[2]])
    # Transition rate
    transition.rates[[3]] = c(transition.rates[[3]],transition.objects[[3]])
    # Source of disease (in the case of transmissions)
    transition.rates[[4]] = c(transition.rates[[4]],transition.objects[[4]])

    return(transition.rates)
  }

  update_rate = function(state_vector, control_matrix, withinCatchmentMovements.objects) {

    # Setup objects containing:
    # 1. contacts between sites
    # 2. sites whose control status is new
    # 3. contacts made out of controlled catchments

    ######## This mechanism allow us to introduce a nationwide standstill or prevent movements from sites with top 10 in or out movements
    ######## This can either be based on the total number of infections or whether there are any sites currently infected
    #if (sum(cumulativeState_vector) >1 ) {
    #  contactp = graph.contactpalt.objects[[3]]
    #}  else {
    #  contactp = graph.contactp.objects[[3]]
    #}


    #if (sum(control_matrix[,c(2,4,5]) >1) {
    #  contactp = graph.contactpalt.objects[[3]]
    #}  else {
    #  contactp = graph.contactp.objects[[3]]
    #}


    contactp = graph.contactp.objects[[3]]
    transition.rates = c(list(NULL),list(NULL),list(NULL),list(NULL))

    # When a site has been contact traced it will be subject to movement controls, but
    # will not be culled, released from controls or used to infer which catchments
    # need to be controlled until infection has been confirmed.
    # Hence contact tracing isn't used to calculate 'movement.restrictions.bySite'

    ######## sites in the surveillance and fallow periods
    movement.restrictions.bySite = as.logical(control_matrix[,c(2,3,4,5)] %*% rep(1,4))

    ####### sites in the surveillance period - different for culling and surveillance scenario as all sites in culling scenario transition from 2 to 4
    movement.restrictions.allSite = as.logical(control_matrix[,c(2,3)] %*% rep(1,2))

    ####### Include sites in surveillance stage 3 as they are allowed to move fish within infected catchments
    transport.onSite.prevented = as.logical(control_matrix[,c(2,4,5,6,7)] %*% rep(1,5))
    transport.offSite.prevented = as.logical(control_matrix[,c(2,4,5,7)] %*% rep(1,4))
    infected.sites.withRecovery = !as.logical(control_matrix[,c(4,5,6)] %*% rep(1,3))
    spread.onSite.prevented = as.logical(control_matrix[,c(4,5,6)] %*% rep(1,3))
    spread.offSite.prevented = spread.onSite.prevented

    clinical.vector = state_vector*!control_matrix[,6]

    # Identify contacts originating from infected sites,
    # excluding contacts from sites that can not transport off site
    atriskcontacts = contactp * (state_vector * !transport.offSite.prevented)

    # Identify, and remove contacts ending at controlled sites,
    # excluding contacts from sites that can not receive transported stuff
    atriskcontacts = t(atriskcontacts) * !transport.onSite.prevented
    atriskcontacts = t(atriskcontacts)

    withinCatchmentMovements.out.objects = aquanet::excludeWithinCatchmentMovements(move_restricted_sites = movement.restrictions.bySite,
                                                                                    spmatrix_risk_contacts = atriskcontacts,
                                                                                    catchment_movements = withinCatchmentMovements.objects,
                                                                                    matrix_movements_prob = contactp)
    atriskcontacts = withinCatchmentMovements.out.objects[[1]]
    withinCatchmentMovements.objects = withinCatchmentMovements.out.objects[[2]]

    # Create an edge list for live fish movements from infected to exposed sites
    susceptable.sites.exposure.rate.objects = aquanet::listRatesSusceptibleRiskContacts(spmatrix_risk_contacts = atriskcontacts,
                                                                                        state_vector = state_vector,
                                                                                        trans_type = 0)
    transition.rates = combineTransitions(susceptable.sites.exposure.rate.objects, transition.rates)

    ######## An attempt to make separate the parameters between the fisheries and farms
    ######## Here i will calculate the transition from infected to subclinical for the two types of site separately
    ## So this first one is for the farms using a new infection period that is less than the fisheries
    # Identify any infected sites, which are not latent, or fallow
    # Create a vector showing the position of infected sites
    # Create a vector with the rate at which sites lapse into latency, or recover
    # State 3 and 2 leads to recovery and latency, respectively
    infected.sites = state_vector * infected.sites.withRecovery*farm_vector
    infected.sites.recover.rate.objects = aquanet::listTransitionRates(run_time_params = ListRunTimeParameters,
                                                                       state_vector = infected.sites,
                                                                       trans_type = "Site_Recovers",
                                                                       site_indices = site.index)
    transition.rates = combineTransitions(infected.sites.recover.rate.objects, transition.rates)
    ########

    ######## And this second is for the fisheries
    # Identify any infected sites, which are not latent, or fallow
    # Create a vector showing the position of infected sites
    # Create a vector with the rate at which sites lapse into latency, or recover
    # State 3 and 2 leads to recovery and latency, respectively
    infected.sites = state_vector * infected.sites.withRecovery*!farm_vector
    infected.sites.recover.rate.objects = aquanet::listTransitionRates(run_time_params = ListRunTimeParameters,
                                                                       state_vector = infected.sites,
                                                                       trans_type = "Infection_Becomes_Subclinical",
                                                                       site_indices = site.index)
    transition.rates = combineTransitions(infected.sites.recover.rate.objects, transition.rates)
    ########

    ######## This is the original code
    # Identify any infected sites, which are not latent, or fallow
    # Create a vector showing the position of infected sites
    # Create a vector with the rate at which sites lapse into latency, or recover
    # State 3 and 2 leads to recovery and latency, respectively
    #infected.sites = state_vector * infected.sites.withRecovery
    #infected.sites.recover.rate.objects = listTransitionRates(infected.sites, 2, site.index, 1)
    #transition.rates = combineTransitions(infected.sites.recover.rate.objects, transition.rates)
    ########

    # Identify any latent, infected sites
    # Create a vector showing the position of latent sites
    # Create a vector with the recovery rate of latent sites
    latent.sites = as.logical(control_matrix[,6])
    latent.sites.recovery.rate.objects = aquanet::listTransitionRates(run_time_params = ListRunTimeParameters,
                                                                      state_vector = latent.sites,
                                                                      trans_type = "Clearing_Of_Latency_From_Infected_Sites",
                                                                      site_indices = site.index)
    transition.rates = combineTransitions(latent.sites.recovery.rate.objects, transition.rates)

    # Identify sites that are fallow, and infected
    # Create a vector showing the position of sites that are fallow and infected
    # Create a vector showing the rate at which fallow sites are disinfected
    fallow.infected.sites = state_vector * (control_matrix[,4] + control_matrix[,5])
    fallow.infected.sites.rate.disinfection = aquanet::listTransitionRates(run_time_params = ListRunTimeParameters,
                                                                           state_vector = fallow.infected.sites,
                                                                           trans_type = "Reinfection_After_Restocking_Const",
                                                                           site_indices = site.index)
    transition.rates = combineTransitions(fallow.infected.sites.rate.disinfection, transition.rates)

    # Identify sites which have been contact traced
    # Create a vector showing the position of sites which have been contact traced
    # Create a vector showing the rate at which contact traced sites will be tested
    contact.traced.sites = control_matrix[,7]
    contact.traced.sites.rate.testing = aquanet::listTransitionRates(run_time_params = ListRunTimeParameters,
                                                                     state_vector = contact.traced.sites,
                                                                     trans_type = "Contact_Detection",
                                                                     site_indices = site.index)
    transition.rates = combineTransitions(contact.traced.sites.rate.testing, transition.rates)

    ########
    # Identify sites that are under surveillance or management
    # This is so the simulation continues even if there is no infection left in the system
    # The simualiton will check roughly every 42 days to see if the management status' need updating
    # Create a vector showing the position of sites that are under surveillance
    # Create a vector showing the rates at which the sites under surveillance should be allowed to trade again
    #surveillance.sites = as.logical(control_matrix[,c(2,3,4,5)] %*% rep(1,4))
    #surveillance.sites = ifelse(surveillance.sites >0, 1, 0)
    #surveillance.sites.rate = listTransitionRates(surveillance.sites, 7, site.index, 1)
    #transition.rates = combineTransitions(surveillance.sites.rate, transition.rates)
    ########

    # Identify sites that can become controlled
    # Create a vector showing the position of sites that can be controlled
    # Create a vector with the control rate of infected sites
    infected.sites.notControlled = control_matrix[,1]
    infected.sites.control.rate.objects = aquanet::listTransitionRates(run_time_params = ListRunTimeParameters,
                                                                       state_vector = infected.sites.notControlled,
                                                                       trans_type = "Detection_Reporting_Disease",
                                                                       site_indices = site.index)
    transition.rates = combineTransitions(infected.sites.control.rate.objects, transition.rates)

    if (winter == FALSE) {
      # Identify any latent, infected sites
      # Create a vector showing the position of latent sites
      # Create a vector with the rate at which sites revert to active expression of disease
      latent.sites.secondOutbreak = aquanet::listTransitionRates(run_time_params = ListRunTimeParameters,
                                                                 state_vector = latent.sites,
                                                                 trans_type = "Second_Outbreak_Due_To_Subclinical_Infection",
                                                                 site_indices = site.index)
      transition.rates = combineTransitions(latent.sites.secondOutbreak, transition.rates)


      # Calculate the probability of a contact occuring downstream of an outbreak, through the river network
      graph.riverDownstream.objects = graph.riverDistance.objects[[1]]
      riverDownstream.matrix = graph.riverDownstream.objects[[2]]
      susceptable.sites.exposure.byRiver.downstream.objects = aquanet::calcRiverTransmission(matrix_river_distances_prob = riverDownstream.matrix,
                                                                                             clinical_state_vector = clinical.vector,
                                                                                             spread_restricted_off = spread.offSite.prevented,
                                                                                             spread_restricted_on = spread.onSite.prevented,
                                                                                             trans_type = 10)
      transition.rates = combineTransitions(susceptable.sites.exposure.byRiver.downstream.objects, transition.rates)

      ########
      ######## Calculate the probability of a contact occuring due to local fomite transmission
      fomite.matrix = graph.estimateSiteDistances.objects[[2]]
      susceptable.sites.exposure.byFomites.objects = aquanet::calcRiverTransmission(matrix_river_distances_prob = fomite.matrix,
                                                                                    clinical_state_vector = clinical.vector,
                                                                                    spread_restricted_off = spread.offSite.prevented,
                                                                                    spread_restricted_on = spread.onSite.prevented,
                                                                                    trans_type = 14)
      transition.rates = combineTransitions(susceptable.sites.exposure.byFomites.objects, transition.rates)
      ########


      #Identify potential transitions from infected to susceptable sites that could occur randomly, regardless of the proposed mechanism
      #Exclude contacts from sites that can not perticipate in such a mechanism of transmition
      if (sum(!state_vector & !spread.onSite.prevented) != 0) {
        spill.over.objects = aquanet::calcRandomSpillover(clinical_state_vector = clinical.vector,
                                                          spread_restricted_off = spread.offSite.prevented,
                                                          spread_restricted_on = spread.onSite.prevented,
                                                          site_indices = site.index,
                                                          trans_type = "Fomite_Transmission_Independant_Prob",
                                                          run_time_params = ListRunTimeParameters)
        transition.rates = combineTransitions(spill.over.objects, transition.rates)
      }
    }

    # Identify sites that may become fallow
    # Create a vector showing the position of sites that can become fallow
    # Create a vector with the rate at which sites become fallow
    controlled.farms = movement.restrictions.allSite * culling_vector
    controlled.sites.fallow.rate.objects = aquanet::listTransitionRates(run_time_params = ListRunTimeParameters,
                                                                        state_vector = controlled.farms,
                                                                        trans_type = "Time_Required_Cull_Site",
                                                                        site_indices = site.index)
    transition.rates = combineTransitions(controlled.sites.fallow.rate.objects, transition.rates)

    return(list(transition.rates, withinCatchmentMovements.objects, movement.restrictions.bySite))
  }

  do_event = function(state_vector, control_matrix, transition.rates, tdiff,  movement.restrictions.bySite, catchment_time_vector, catchments.all.sites.c5.status, record_transition_times, source.infection.vector, infected.source.matrix) {

    controlled.sites.c4.logical = as.logical(control_matrix[, 4])

    # Update vector showing time since application of controls
    # Only start counting when the site has recovered
    # Time the period over which controls are applied, as well as the period a site has been fallow (these cases are mutally exclusive)
    surveillance.noVisibleInfection = (movement.restrictions.bySite & !state_vector & !control_matrix[,6]) | (movement.restrictions.bySite & state_vector & control_matrix[,6]) | controlled.sites.c4.logical
    time_vector[surveillance.noVisibleInfection] = time_vector[surveillance.noVisibleInfection] + tdiff

    # Increment the time recorded since every site in a catchment has been ready to be restocked
    catchment_time_vector[catchments.all.sites.c5.status] = catchment_time_vector[catchments.all.sites.c5.status] + tdiff

    # Vector of possible events, expressed as the site to be infected if an event occurred
    site.vector=transition.rates[[2]]

    # Calculate the total rate
    rate.total = sum(transition.rates[[3]])

    # Create a vector of probabilities
    p = transition.rates[[3]] / rate.total
    no.rates = length(p)

    # Pick an event number, from those available, using the vector of probabilities
    if (no.rates != 1) {
      event = sample.int(no.rates, size=1, prob = p, replace = TRUE)
    } else {
      event = 1
    }

    # Check the site corresponding to a particular event
    site = site.vector[event] + 1

    # Lookup the event number and type, and modify the state appropriately
    rate.type = transition.rates[[1]][event]

    ##### S --> I | L --> I  ####
    if (rate.type %in% c(0, 4, 10, 11, 14)) {

      # Note the site is in an infectious state
      state_vector[site] = 1

      # If it is winter, identify the site as latent
      if (winter == TRUE) {
        control_matrix[site, 6] = 1
        state_vector[site] = 1
      }

      # Identify the site as having infection which can potentially be detected
      else {

        # Lookup the source of the infection and record it, in case contact tracing
        # needs to be applied at a later point
        if (sum(control_matrix[site, 2:5]) == 0) {
          control_matrix[site, 1] = 1

          if (rate.type %in% c(0,10)) {
            source.infection = transition.rates[[4]][event] + 1
            source.infection.vector[site] = source.infection
            ########
            infected.source.matrix[source.infection,  site] = 1
          }
        }

        # Note the site is no longer latent
        control_matrix[site, 6] = 0
      }
    }

    # I --> L - for both farm and fishery
    if (rate.type %in% c(2,3)) {
      # If the site has been infected, but has recovered, before being placed under control, assume that it will not be controlled
      # If the site is controlled, and has recovered, reset the clock
      if (control_matrix[site, 1] == 1) {
        control_matrix[site, 1] = 0
      } else if (sum(control_matrix[site, c(2,3)]) == 1) {
        time_vector[site] = 0
      }

      control_matrix[site, 6] = 1
    }

    # I --> C transition | Traced Site --> C transition
    if (rate.type %in% c(6, 12)) {
      # Note the site no longer needs to be contact traced,
      # since it has been tested through other means
      if (state_vector[site] == 1 && control_matrix[site, 7] == 1) {
        control_matrix[site, 2] = 1
        control_matrix[site, c(1,3,7)] = 0
      }

      control_matrix[site, 7] = 0


      # Note the site is now in a controlled state, provided it is still infected
      # Clock is only reset when site is recovered, not when it is placed under control
      if (state_vector[site] == 1 && control_matrix[site, 6] == 0) {
        control_matrix[site, 2] = 1
        control_matrix[site, c(1,3)] = 0
      }

      # Note any sites which have been in contact with
      # the infected site and which transmitted infection
      # via live fish movements or river-based transmission
      source.infection = source.infection.vector[site]

      if (source.infection != 0) {
        source.infection.vector[site] = 0

        # Don't test a site for infection if it has already
        # been subject to controls
        if (sum(control_matrix[source.infection, 2:5]) == 0) {
          control_matrix[source.infection, 7] = 1
        }
      }

      ######## Forward Tracing
      # Note any sites which have been in contacted by
      # the infected site and which transmitted infection
      # via live fish movements or river-based transmission
      infected.source = infected.source.matrix[site,]
      infected.sites = which(infected.source == 1)


      if(sum(infected.source) != 0){
        infected.source.matrix[site,] = 0

        for(i in 1:length(infected.sites)){
          # Don't test a site for infection if it has already
          # been subject to controls
          source.site = infected.sites[i]
          if(sum(control_matrix[source.site, 2:5]) == 0) {
            control_matrix[source.site,  7] = 1
          }
        }
      }

      ########
    }

    # C --> F transition
    if (rate.type == 9) {
      # Note the site is now fallow
      control_matrix[site, 4] = 1
      control_matrix[site, c(2,3)] = 0

      # Note the site is no longer latent
      control_matrix[site, 6] = 0

      # Reset the time on the clock, so that it is possible to check how long the site has been fallow
      # A site can not have multiple control states at the same time, so this should not interfere with other code
      time_vector[site] = 0

      if (winter == TRUE) {
        state_vector[site] = 0
      }
    }

    # L -> S transition
    if (rate.type == 5) {
      # Note S state
      state_vector[site] = 0

      # Note the site is no longer latent
      control_matrix[site, 6] = 0

      time_vector[site] = 0

    }

    # F,I --> F,S transition (decontamination of fallow sites)
    if (rate.type == 1) {
      # Note S state
      state_vector[site] = 0

    }

    # Update controls on those sites which have passed a given no. days without infection
    ## min.trans = ListRunTimeParameters[[7]]
    min.trans = 720
    controlled.sites.c2.logical = as.logical(control_matrix[, 2]*!state_vector)
    allow.inward.movements = (time_vector > min.trans) & controlled.sites.c2.logical
    allow.inward.movements.no = sum(allow.inward.movements)

    if (allow.inward.movements.no != 0) {
      control_matrix[allow.inward.movements, 2] = 0
      control_matrix[allow.inward.movements, 3] = 1
    }

    # Update controls on those sites which have passed a given no. days without infection
    min.trans = 360 + ListRunTimeParameters[[8]]
    controlled.sites.c3.logical = as.logical(control_matrix[, 3])
    allow.all.movements = (time_vector > min.trans) & controlled.sites.c3.logical
    allow.all.movements.no = sum(allow.all.movements)

    if (allow.all.movements.no != 0) {
      control_matrix[allow.all.movements, 3] = 0
      time_vector[allow.all.movements] = 0
    }

    # Update controls on those sites which have been fallow for more than x number of days
    min.trans = ListRunTimeParameters[[10]]
    recover.site = (time_vector > min.trans) & controlled.sites.c4.logical
    recover.site.no = sum(recover.site)

    if (recover.site.no != 0) {
      control_matrix[recover.site, 4] = 0
      control_matrix[recover.site, 5] = 1

      checkCatchmentLevelRestocking.objects = checkCatchmentLevelRestocking(control_matrix, tdiff)
      control_matrix = checkCatchmentLevelRestocking.objects[[1]]
      catchments.some.sites.c4.status = checkCatchmentLevelRestocking.objects[[2]]
      catchments.all.sites.c5.status = checkCatchmentLevelRestocking.objects[[3]]
      catchment_time_vector[catchments.some.sites.c4.status] = 0
    }

    # Identify catchments where every site has been ready to be restocked, for more than four days
    catchments.ready.restock = rep(FALSE, no.catchments)
    catchments.ready.restock[catchments.all.sites.c5.status] = catchment_time_vector[catchments.all.sites.c5.status] >= 4
    no.catchments.ready.restock = sum(as.numeric(catchments.ready.restock))

    # Print information on catchments where every site has been ready to be restocked, for more than four days
    if (no.catchments.ready.restock > 0) {
      sitesReadyRestocked = as.logical((graph.catchment2site.matrix2 * control_matrix[,5]) %*% catchments.ready.restock)

      control_matrix[sitesReadyRestocked, 5] = 0
      ###
      #control_matrix[sitesReadyRestocked, 3] = 1
      ###

      catchments.all.sites.c5.status[catchments.ready.restock] = FALSE
      catchment_time_vector[catchments.ready.restock] = 0

      time_vector[sitesReadyRestocked] = 0
    }

    return(list(state_vector, control_matrix, time_vector, catchment_time_vector, catchments.all.sites.c5.status, record_transition_times, source.infection.vector, rate.type, infected.source.matrix))
  }

  commitResults = function(allStates.table, allStates.table.t, numberFullSaves) {
    allStates.matrix = as(object = as.matrix(allStates.table[((no.variables + 1):(no.variables + contactp.length)),]), Class = "dgTMatrix")

    simStates.longTable = data.frame(as.integer(site.index[(allStates.matrix@i + 1)] + 1),
                                     as.integer(allStates.matrix@x),
                                     as.integer(allStates.matrix@j + ((numberFullSaves - 1) * commitInterval)),
                                     as.integer(allStates.table[3,])[allStates.matrix@j + 1])

    colnames(simStates.longTable) = c('siteID','state','timeID','simNo')

    simTimes.longTable = data.frame(as.integer(iterationID.vector + ((numberFullSaves - 1) * commitInterval)),
                                    as.integer(allStates.table[3,])[iterationID.vector],
                                    as.numeric(allStates.table.t[1,])[iterationID.vector],
                                    as.numeric(allStates.table.t[2,])[iterationID.vector])

    colnames(simTimes.longTable) = c('timeID','simNo','tdiff','t')


    save(simStates.longTable, simTimes.longTable, file = paste(locationSaveResults,"/FullDetails/batchNo-",batchNo,"_simNo-",simNo,"_NoCommits-",numberFullSaves,".RData",sep=""),compress=FALSE)
  }

  for (k in 1:runs) {
    # Calculate a simulation number, which is equivilent to k, but valid across every thread / process
    simNo = k + ((batchNo - 1) * runs)

    # Record the current time,
    # the time difference between two steps in the simulation,
    t = 0
    tdiff = 0
    rate.type = 0

    # Create empty vectors to record the time since a catchment's status was last changed,
    # whether there are any fallow sites in a catchment,
    # or whether all the sites in a catchment are ready to be restocked
    catchment_time_vector = rep(0, length = no.catchments)
    catchments.some.sites.c4.status = rep(0, length = no.catchments)
    catchments.all.sites.c5.status = rep(0, length = no.catchments)

    # Create empty vectors to record a site's state of infection, control,
    # and how long it has been in a specific state of infection or control
    state_vector = rep(0, contactp.length)
    cumulativeState_vector = state_vector
    farmcumulativeState_vector = state_vector*farm_vector
    #farmcumulativeState_vector = state_vector*mediumfish_vector
    fisherycumulativeState_vector = state_vector*as.numeric(!farm_vector)
    control_matrix = matrix(data = 0, nrow = contactp.length, ncol = 7)
    time_vector = rep(0, contactp.length)

    # Create a vector to track which site was responsible for infection (when infection
    # was transmitted via live fish movements or the river network)
    source.infection.vector = rep(0, contactp.length)

    ######## Create a matrix to track the sites are infected by certain sites via LFM or river network - This is for forward contact tracing
    infected.source.matrix = matrix(data = 0, nrow = contactp.length, ncol = contactp.length)
    ########

    # Save the list of contacts that were effected by catchment level restrictions in the previous time step
    listContacts.exclude = new(Class = "dgTMatrix", Dim = c(contactp.length,contactp.length))

    # Save the list of catchments and sites which were controlled in the previous time-step,
    # to avoid expensive recalculation
    controlled.catchments.previous = vector(mode = "numeric", length = no.catchments)
    controlled.catchments.previous = as(object = controlled.catchments.previous, Class = "dgeMatrix")
    secondary.controlled.sites = vector(mode = "logical", length = contactp.length)
    no.controlled.catchments = 0

    withinCatchmentMovements.objects = list(graph.catchment2site.matrix2, graph.withinCatchmentEdges.matrix, controlled.catchments.previous, listContacts.exclude, associatedSiteControlType, secondary.controlled.sites,no.controlled.catchments)

    ######## Pick the first infected site, at random, and update it's recorded status appropriately - these are all seeded at farms
    d = 0
    farm.select <- c()

    for(d in 0:length(farm_vector)){
      d = d + 1
      value = farm_vector[d]*d
      farm.select = c(farm.select,value)
    }

    farm.select <- as.vector(na.omit(farm.select))
    farm.select <- subset(farm.select, farm.select > 0)
    primary.event = sample(farm.select,1)

    state_vector[primary.event] = 1
    noSusceptibleSites = sum(!state_vector)
    ########


    ######## Produce a vector for culling a random number of fisheries
    culling <- ifelse(farm_vector == 1, 0, runif(length(farm_vector)))
    culling_vector <- ifelse(culling < 0.5, 1, 0)
    ########

    while(t<tmax){

      ## Select seasonality periodicity
      #winter = (((t %/% 180) %% 2 ) == 1)  # Winter 180 days, Summer 180 days, Winter occurs first
      winter = (((t %/% 90) %% 4 ) == 3)
      # winter = (((t %/% 90) %% 4 ) == 1) & (((t %/% 90) %% 4 ) == 3)
      # winter = FALSE

      # Update the list of transitions
      update_rate.output.objects = update_rate(state_vector, control_matrix, withinCatchmentMovements.objects)

      # List of every transition
      transition.rates = update_rate.output.objects[[1]]

      # Cache any calculations on movements out of controlled catchments, to avoid unnecesary recalculation
      withinCatchmentMovements.objects = update_rate.output.objects[[2]]
      secondary.controlled.sites = withinCatchmentMovements.objects[[6]]

      # Retrieve logical vectors for each type of controlled state, to avoid recalculation
      movement.restrictions.bySite = update_rate.output.objects[[3]]

      # Combine all of the site's attributes into a single state, count the total number of sites per state
      combinedStates_vector = as.integer((state_vector * 10) + (secondary.controlled.sites * 20) + (control_matrix[,2:6] %*% 2:6) + control_matrix[,7])
      cumulativeState_vector = (state_vector | cumulativeState_vector)
      farmStates.vector = farm_vector*state_vector
      farmcumulativeState_vector = (farmStates.vector | farmcumulativeState_vector)
      fisheriesStates.vector = state_vector*as.numeric(!farm_vector)
      fisherycumulativeState_vector = (fisheriesStates.vector | fisherycumulativeState_vector)

      combfarm.vector = farm_vector*combinedStates_vector
      comfishery.vector = as.numeric(!farm_vector)*combinedStates_vector
      combinedStates.total = tabulate(combinedStates_vector, nbins = no.variables)
      farmcombinedstates.total = tabulate(combfarm.vector, nbins = no.variables)
      fisheriescombinedstates.total = tabulate(comfishery.vector, nbins = no.variables)

      noOperations = noOperations + 1
      no.controlled.catchments = withinCatchmentMovements.objects[[7]]
      set(x = summaryStates.table, j = as.character(noOperations), value = c(batchNo,k, t, tdiff, simNo, rate.type, no.controlled.catchments, sum(farmcumulativeState_vector), farmcombinedstates.total))

      if (noOperations %% commitInterval == (commitInterval - 1)) {
        summaryStates.table[,as.character((ncol(summaryStates.table) + 1):(ncol(summaryStates.table) + 1 + commitInterval)):=empty.vector.byState]
      }

      # If there are no infectious sites on the network stop the simulation
      if (length(transition.rates[[3]]) == 0) {
        break()
      }

      # Randomly pick next time step, based on a weighted expontial distribution
      tdiff = rexp(1, sum(transition.rates[[3]]))

      t = t + tdiff
      noSteps.sinceLastCommit = noSteps %% commitInterval
      noSteps = noSteps + 1

      # Record the current state of the network, for analysis over all time periods and simulations
      # Make sure that all of the variables stored in the 'allStates.table' are integers (hence the [0-9]L syntax)
      # For analysis outside of the model, treating a site's state as multidimensional is going to be really confusing
      # The following line of code should combine all the site's attributes into a single number,
      # which uniquely represents all of the attributes co-occuring within the same site

      #set(x = allStates.table, i = (no.variables + 1):(no.variables + contactp.length),j = as.character(noSteps.sinceLastCommit + 1), value = as.integer(combinedStates_vector))
      #set(x = allStates.table, i = (1:(no.variables + 3)), j = as.character(noSteps.sinceLastCommit + 1), value = as.integer(c(batchNo, k, k + ((batchNo - 1) * runs), combinedStates.total)))
      #set(x = allStates.table.t, j = as.character(noSteps.sinceLastCommit + 1), value = c(tdiff, t - tdiff))


      # Save the results to disk
      #if (noSteps.sinceLastCommit == (commitInterval - 1)) {
      #  numberFullSaves = noSteps %/% commitInterval
      #  commitResults(allStates.table, allStates.table.t, numberFullSaves)
      #  allStates.table[,as.character(iterationID.vector):=empty.vector]
      #  allStates.table.t[,as.character(iterationID.vector):=empty.vector.t]
      #}

      # Pick the next event, and modify a site's state accordingly
      event.objects = do_event(state_vector, control_matrix, transition.rates, tdiff, movement.restrictions.bySite, catchment_time_vector, catchments.all.sites.c5.status, record_transition_times, source.infection.vector, infected.source.matrix)

      state_vector = event.objects[[1]]
      control_matrix = event.objects[[2]]
      time_vector = event.objects[[3]]
      catchment_time_vector = event.objects[[4]]
      catchments.all.sites.c5.status = event.objects[[5]]
      record_transition_times = event.objects[[6]]
      source.infection.vector = event.objects[[7]]
      rate.type = event.objects[[8]]
      ########
      infected.source.matrix = event.objects[[9]]
      ########

      if (noSteps%%100 == 1) {
        print(c(k,noSteps,length(state_vector),sum(state_vector),tdiff,length(transition.rates[[3]])))
      }
    }


  }

  # Print diagnositic information, and format results as appriopriate
  print(c("No Iterations", noSteps))

  #allStates.table[,as.character((noSteps.sinceLastCommit + 1):commitInterval):=NULL]
  #allStates.table.t[,as.character((noSteps.sinceLastCommit + 1):commitInterval):=NULL]
  #numberFullSaves = numberFullSaves + 1
  #commitResults(allStates.table, allStates.table.t, numberFullSaves)

  save(summaryStates.table, file = paste(locationSaveResults,"/Summary/batchNo-",batchNo,".RData",sep=""),compress=FALSE)

  return(batchNo)
}
