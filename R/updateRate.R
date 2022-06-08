update_rate <- function(state_vector, control_matrix, withinCatchmentMovements.objects,
                        matrix_movements_prob) {

  # Setup objects containing:
  # 1. contacts between sites
  # 2. sites whose control status is new
  # 3. contacts made out of controlled catchments

  # create empty list for transition rate storage
  trans_rates <- vector(mode = "list", length = 4)

  # When a site has been contact traced it will be subject to movement controls, but
  # will not be culled, released from controls or used to infer which catchments
  # need to be controlled until infection has been confirmed.
  # Hence contact tracing isn't used to calculate 'movement.restrictions.bySite'

  ######## sites in the surveillance and fallow periods
  movement.restrictions.bySite <- as.logical(control_matrix[,c(2,3,4,5)] %*% rep(1,4))

  ####### sites in the surveillance period - different for culling and surveillance scenario as all sites in culling scenario transition from 2 to 4
  movement.restrictions.allSite <- as.logical(control_matrix[,c(2,3)] %*% rep(1,2))

  ####### Include sites in surveillance stage 3 as they are allowed to move fish within infected catchments
  transport.onSite.prevented <- as.logical(control_matrix[,c(2,4,5,6,7)] %*% rep(1,5))
  transport.offSite.prevented <- as.logical(control_matrix[,c(2,4,5,7)] %*% rep(1,4))
  infected.sites.withRecovery <- !as.logical(control_matrix[,c(4,5,6)] %*% rep(1,3))
  spread.onSite.prevented <- as.logical(control_matrix[,c(4,5,6)] %*% rep(1,3))
  spread.offSite.prevented <- spread.onSite.prevented

  clinical.vector <- state_vector*!control_matrix[,6]

  # Identify contacts originating from infected sites,
  # excluding contacts from sites that can not transport off site
  atriskcontacts <- matrix_movements_prob * (state_vector * !transport.offSite.prevented)

  # Identify, and remove contacts ending at controlled sites,
  # excluding contacts from sites that can not receive transported stuff
  atriskcontacts <- t(atriskcontacts) * !transport.onSite.prevented
  atriskcontacts <- t(atriskcontacts)

  withinCatchmentMovements.out.objects <- excludeWithinCatchmentMovements(movement.restrictions.bySite, atriskcontacts, withinCatchmentMovements.objects)
  atriskcontacts <- withinCatchmentMovements.out.objects[[1]]
  withinCatchmentMovements.objects <- withinCatchmentMovements.out.objects[[2]]

  # Create an edge list for live fish movements from infected to exposed sites
  susceptable.sites.exposure.rate.objects <- listInfectionRates(atriskcontacts, state_vector, 0)
  trans_rates <- combineTransitions(susceptable.sites.exposure.rate.objects, trans_rates)

  ######## An attempt to make separate the parameters between the fisheries and farms
  ######## Here i will calculate the transition from infected to subclinical for the two types of site separately
  ## So this first one is for the farms using a new infection period that is less than the fisheries
  # Identify any infected sites, which are not latent, or fallow
  # Create a vector showing the position of infected sites
  # Create a vector with the rate at which sites lapse into latency, or recover
  # State 3 and 2 leads to recovery and latency, respectively
  infected.sites <- state_vector * infected.sites.withRecovery*farm_vector
  infected.sites.recover.rate.objects <- listTransitionRates(infected.sites, 3, site.index, 1)
  trans_rates <- combineTransitions(infected.sites.recover.rate.objects, trans_rates)
  ########

  ######## And this second is for the fisheries
  # Identify any infected sites, which are not latent, or fallow
  # Create a vector showing the position of infected sites
  # Create a vector with the rate at which sites lapse into latency, or recover
  # State 3 and 2 leads to recovery and latency, respectively
  infected.sites <- state_vector * infected.sites.withRecovery*!farm_vector
  infected.sites.recover.rate.objects <- listTransitionRates(infected.sites, 2, site.index, 1)
  trans_rates <- combineTransitions(infected.sites.recover.rate.objects, trans_rates)
  ########

  ######## This is the original code
  # Identify any infected sites, which are not latent, or fallow
  # Create a vector showing the position of infected sites
  # Create a vector with the rate at which sites lapse into latency, or recover
  # State 3 and 2 leads to recovery and latency, respectively
  #infected.sites <- state_vector * infected.sites.withRecovery
  #infected.sites.recover.rate.objects <- listTransitionRates(infected.sites, 2, site.index, 1)
  #trans_rates <- combineTransitions(infected.sites.recover.rate.objects, trans_rates)
  ########

  # Identify any latent, infected sites
  # Create a vector showing the position of latent sites
  # Create a vector with the recovery rate of latent sites
  latent.sites <- as.logical(control_matrix[,6])
  latent.sites.recovery.rate.objects <- listTransitionRates(latent.sites, 5, site.index, 1)
  trans_rates <- combineTransitions(latent.sites.recovery.rate.objects, trans_rates)

  # Identify sites that are fallow, and infected
  # Create a vector showing the position of sites that are fallow and infected
  # Create a vector showing the rate at which fallow sites are disinfected
  fallow.infected.sites <- state_vector * (control_matrix[,4] + control_matrix[,5])
  fallow.infected.sites.rate.disinfection <- listTransitionRates(fallow.infected.sites, 1, site.index, 1)
  trans_rates <- combineTransitions(fallow.infected.sites.rate.disinfection, trans_rates)

  # Identify sites which have been contact traced
  # Create a vector showing the position of sites which have been contact traced
  # Create a vector showing the rate at which contact traced sites will be tested
  contact.traced.sites <- control_matrix[,7]
  contact.traced.sites.rate.testing <- listTransitionRates(contact.traced.sites, 12, site.index, 1)
  trans_rates <- combineTransitions(contact.traced.sites.rate.testing, trans_rates)

  ########
  # Identify sites that are under surveillance or management
  # This is so the simulation continues even if there is no infection left in the system
  # The simualiton will check roughly every 42 days to see if the management status' need updating
  # Create a vector showing the position of sites that are under surveillance
  # Create a vector showing the rates at which the sites under surveillance should be allowed to trade again
  #surveillance.sites <- as.logical(control_matrix[,c(2,3,4,5)] %*% rep(1,4))
  #surveillance.sites <- ifelse(surveillance.sites >0, 1, 0)
  #surveillance.sites.rate <- listTransitionRates(surveillance.sites, 7, site.index, 1)
  #trans_rates <- combineTransitions(surveillance.sites.rate, trans_rates)
  ########

  # Identify sites that can become controlled
  # Create a vector showing the position of sites that can be controlled
  # Create a vector with the control rate of infected sites
  infected.sites.notControlled <- control_matrix[,1]
  infected.sites.control.rate.objects <- listTransitionRates(infected.sites.notControlled, 6, site.index, 1)
  trans_rates <- combineTransitions(infected.sites.control.rate.objects, trans_rates)

  if (winter == FALSE) {
    # Identify any latent, infected sites
    # Create a vector showing the position of latent sites
    # Create a vector with the rate at which sites revert to active expression of disease
    latent.sites.secondOutbreak <- listTransitionRates(latent.sites, 4, site.index, 1)
    trans_rates <- combineTransitions(latent.sites.secondOutbreak, trans_rates)


    # Calculate the probability of a contact occuring downstream of an outbreak, through the river network
    graph.riverDownstream.objects <- graph.riverDistance.objects[[1]]
    riverDownstream.matrix <- graph.riverDownstream.objects[[2]]
    susceptable.sites.exposure.byRiver.downstream.objects <- calcRiverTransmission(riverDownstream.matrix, clinical.vector, spread.offSite.prevented, spread.onSite.prevented, 10)
    trans_rates <- combineTransitions(susceptable.sites.exposure.byRiver.downstream.objects, trans_rates)

    ########
    ######## Calculate the probability of a contact occuring due to local fomite transmission
    fomite.matrix <- graph.estimateSiteDistances.objects[[2]]
    susceptable.sites.exposure.byFomites.objects <- calcRiverTransmission(fomite.matrix, clinical.vector, spread.offSite.prevented, spread.onSite.prevented, 14)
    trans_rates <- combineTransitions(susceptable.sites.exposure.byFomites.objects, trans_rates)
    ########


    #Identify potential transitions from infected to susceptable sites that could occur randomly, regardless of the proposed mechanism
    #Exclude contacts from sites that can not perticipate in such a mechanism of transmition
    if (sum(!state_vector & !spread.onSite.prevented) != 0) {
      spill.over.objects <- calcRandomSpillover(clinical.vector, spread.offSite.prevented, spread.onSite.prevented, 11)
      trans_rates <- combineTransitions(spill.over.objects, trans_rates)
    }
  }

  # Identify sites that may become fallow
  # Create a vector showing the position of sites that can become fallow
  # Create a vector with the rate at which sites become fallow
  controlled.farms <- movement.restrictions.allSite * culling_vector
  controlled.sites.fallow.rate.objects <- listTransitionRates(controlled.farms, 9, site.index, 1)
  trans_rates <- combineTransitions(controlled.sites.fallow.rate.objects, trans_rates)

  return(list(trans_rates, withinCatchmentMovements.objects, movement.restrictions.bySite))
}
