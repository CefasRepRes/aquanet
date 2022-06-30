#' simulationCode
#'
#' @param graph.contactp.objects TODO
#'
#' @param runs TODO
#'
#' @param tmax TODO
#'
#' @param batchNo TODO
#'
#' @param ListRunTimeParameters TODO
#'
#' @param graph.withinCatchmentEdges.objects TODO
#'
#' @param graph.catchment2Site.objects TODO
#'
#' @param graph.riverDistance.objects TODO
#'
#' @param graph.estimateSiteDistances.objects TODO
#'
#' @param farm_vector TODO
#'
#' @param associatedSiteControlType TODO
#'
#' @param locationSaveResults TODO
#'
#' @param initialNoInfections TODO
#'
#' @return
#'
#' @export
#'
#' @importFrom methods new
#' @importFrom stats na.omit rexp runif
#' @importFrom data.table set data.table :=
simulationCode <- function(graph.contactp.objects,
                           runs,
                           tmax,
                           batchNo,
                           ListRunTimeParameters,
                           graph.withinCatchmentEdges.objects,
                           graph.catchment2Site.objects,
                           graph.riverDistance.objects,
                           graph.estimateSiteDistances.objects,
                           farm_vector,
                           associatedSiteControlType,
                           locationSaveResults,
                           initialNoInfections) {

  ## extract information from input parameters ----

  # Retrieve the contact network, and the number of sites in the network
  n_sites <- graph.contactp.objects[[1]]
  contactp <- graph.contactp.objects[[3]]
  contactp.siteNames <- dimnames(contactp)[[1]]

  # Matrix representing the site / catchment relationship
  graph.catchment2site.matrix2 <- graph.catchment2Site.objects[[2]]

  # Matrix identifying which of the contacts occur between sites in the same catchment
  graph.withinCatchmentEdges.matrix <- graph.withinCatchmentEdges.objects[[1]]

  # Number of catchments within the model0
  n_catchments <- graph.catchment2site.matrix2@Dim[2]


  ## create variables to populate ----

  # record number of steps, operations (including premature termination), and full saves per simulation
  n_steps <- 0
  n_operations <- 0
  n_saves <- 0

  # define interval at which results should be saved
  commitInterval <- 5000
  iterationID.vector <- 1:commitInterval

  # create vector of 0-based site indices (unique positions within matrix)
  site_index <- 0:(n_sites - 1)

  # Number of different combinations of states possible within the model
  no.variables <- 42

    # Create empty records, which are used to force enough memory to be allocated for results
  empty.vector <- rep(0, n_sites + no.variables)
  empty.vector.t <- rep(0, 2)
  empty.vector.byState <- rep(0, no.variables + 8)

  # Preallocate memory for storing results
  allStates.table <- data.table::data.table(empty.vector)
  allStates.table[ , as.character(iterationID.vector) := empty.vector]

  allStates.table.t <- data.table(empty.vector.t)
  allStates.table.t[ , as.character(iterationID.vector) := empty.vector.t]

  summaryStates.table <- data.table(empty.vector.byState)
  summaryStates.table[ , as.character(iterationID.vector) := empty.vector.byState]
  summaryStates.table[ , c("empty.vector.byState") := NULL]


  ## simulate each model run ----

  for (k in 1:runs) {
    # Calculate a simulation number, which is equivilent to k, but valid across every thread / process
    simNo <- k + ((batchNo - 1) * runs)

    # Record the current time,
    # the time difference between two steps in the simulation,
    t <- 0
    tdiff <- 0
    rate.type <- 0

    # Create empty vectors to record the time since a catchment's status was last changed,
    # whether there are any fallow sites in a catchment,
    # or whether all the sites in a catchment are ready to be restocked
    catchment_time_vector <- rep(0, length = n_catchments)
    catchments.some.sites.c4.status <- rep(0, length = n_catchments)
    catchments.all.sites.c5.status <- rep(0, length = n_catchments)

    # Create empty vectors to record a site's state of infection, control,
    # and how long it has been in a specific state of infection or control
    state_vector <- rep(0, n_sites)
    cumulativeState_vector <- state_vector
    farmcumulativeState_vector <- state_vector * farm_vector
    #farmcumulativeState_vector <- state_vector * mediumfish_vector
    fisherycumulativeState_vector <- state_vector * as.numeric(!farm_vector)
    control_matrix <- matrix(data = 0, nrow = n_sites, ncol = 7)
    time_vector <- rep(0, n_sites)

    # Create a vector to track which site was responsible for infection (when infection
    # was transmitted via live fish movements or the river network)
    source.infection.vector <- rep(0, n_sites)

    ######## Create a matrix to track the sites are infected by certain sites via LFM or river network - This is for forward contact tracing
    infected.source.matrix <- matrix(data = 0, nrow = n_sites, ncol = n_sites)

    # Save the list of contacts that were effected by catchment level restrictions in the previous time step
    listContacts.exclude <- methods::new(Class = "dgTMatrix", Dim = c(n_sites, n_sites))

    # Save the list of catchments and sites which were controlled in the previous time-step,
    # to avoid expensive recalculation
    controlled.catchments.previous <- vector(mode = "numeric", length = n_catchments)
    controlled.catchments.previous <- as(object = controlled.catchments.previous, Class = "dgeMatrix")
    secondary.controlled.sites <- vector(mode = "logical", length = n_sites)
    no.controlled.catchments <- 0

    withinCatchmentMovements.objects <- list(graph.catchment2site.matrix2,
                                             graph.withinCatchmentEdges.matrix,
                                             controlled.catchments.previous,
                                             listContacts.exclude,
                                             associatedSiteControlType,
                                             secondary.controlled.sites,
                                             no.controlled.catchments)

    ######## Pick the first infected site, at random, and update it's recorded status appropriately - these are all seeded at farms
    d <- 0
    farm.select <- c()

    for(d in 0:length(farm_vector)){
      d <- d + 1
      value <- farm_vector[d] * d
      farm.select <- c(farm.select, value)
    }

    farm.select <- as.vector(stats::na.omit(farm.select))
    farm.select <- subset(farm.select, farm.select > 0)
    primary.event <- sample(farm.select, 1)

    state_vector[primary.event] <- 1
    noSusceptibleSites <- sum(!state_vector)

    # Produce a vector for culling a random number of fisheries
    culling <- ifelse(farm_vector == 1, 0, stats::runif(length(farm_vector)))
    culling_vector <- ifelse(culling < 0.5, 1, 0)


    while(t<tmax){

      ## Select seasonality periodicity
      #winter <- (((t %/% 180) %% 2 ) == 1)  # Winter 180 days, Summer 180 days, Winter occurs first
      winter <- (((t %/% 90) %% 4 ) == 3)
      # winter <- (((t %/% 90) %% 4 ) == 1) & (((t %/% 90) %% 4 ) == 3)
      # winter <- FALSE

      # Update the list of transitions
      update_rate.output.objects <- aquanet::updateRates(control_matrix = control_matrix,
                                                        state_vector = state_vector,
                                                        farm_vector =  farm_vector,
                                                        culling_vector = culling_vector,
                                                        site_indices = site_index,
                                                        catchment_movements = withinCatchmentMovements.objects,
                                                        movements_prob = graph.contactp.objects,
                                                        river_prob = graph.riverDistance.objects,
                                                        site_distances_prob = graph.estimateSiteDistances.objects,
                                                        run_time_params = ListRunTimeParameters,
                                                        non_peak_season = winter)

      # List of every transition
      transition.rates <- update_rate.output.objects[[1]]

      # Cache any calculations on movements out of controlled catchments, to avoid unnecesary recalculation
      withinCatchmentMovements.objects <- update_rate.output.objects[[2]]
      secondary.controlled.sites <- withinCatchmentMovements.objects[[6]]

      # Retrieve logical vectors for each type of controlled state, to avoid recalculation
      movement.restrictions.bySite <- update_rate.output.objects[[3]]

      # Combine all of the site's attributes into a single state, count the total number of sites per state
      combinedStates_vector <- as.integer((state_vector * 10) +
                                            (secondary.controlled.sites * 20) +
                                            (control_matrix[ , 2:6] %*% 2:6) +
                                            control_matrix[ , 7])
      cumulativeState_vector <- (state_vector | cumulativeState_vector)
      farmStates.vector <- farm_vector * state_vector
      farmcumulativeState_vector <- (farmStates.vector | farmcumulativeState_vector)
      fisheriesStates.vector <- state_vector * as.numeric(!farm_vector)
      fisherycumulativeState_vector <- (fisheriesStates.vector | fisherycumulativeState_vector)

      combfarm.vector <- farm_vector * combinedStates_vector
      comfishery.vector <- as.numeric(!farm_vector) * combinedStates_vector
      combinedStates.total <- tabulate(combinedStates_vector, nbins = no.variables)
      farmcombinedstates.total <- tabulate(combfarm.vector, nbins = no.variables)
      fisheriescombinedstates.total <- tabulate(comfishery.vector, nbins = no.variables)

      n_operations <- n_operations + 1
      no.controlled.catchments <- withinCatchmentMovements.objects[[7]]
      data.table::set(x = summaryStates.table,
                      j = as.character(n_operations),
                      value = c(batchNo,k, t, tdiff, simNo, rate.type, no.controlled.catchments, sum(farmcumulativeState_vector), combinedStates.total))

      if (n_operations %% commitInterval == (commitInterval - 1)) {
        summaryStates.table[ , as.character((ncol(summaryStates.table) + 1):(ncol(summaryStates.table) + 1 + commitInterval)) := empty.vector.byState]
      }

      # If there are no infectious sites on the network stop the simulation
      if (length(transition.rates[[3]]) == 0) {
        break()
      }

      # Randomly pick next time step, based on a weighted expontial distribution
      tdiff <- stats::rexp(1, sum(transition.rates[[3]]))

      t <- t + tdiff
      noSteps.sinceLastCommit <- n_steps %% commitInterval
      n_steps <- n_steps + 1

      # Record the current state of the network, for analysis over all time periods and simulations
      # Make sure that all of the variables stored in the 'allStates.table' are integers (hence the [0-9]L syntax)
      # For analysis outside of the model, treating a site's state as multidimensional is going to be really confusing
      # The following line of code should combine all the site's attributes into a single number,
      # which uniquely represents all of the attributes co-occuring within the same site

      #data.table::set(x = allStates.table, i = (no.variables + 1):(no.variables + n_sites),j = as.character(noSteps.sinceLastCommit + 1), value = as.integer(combinedStates_vector))
      #data.table::set(x = allStates.table, i = (1:(no.variables + 3)), j = as.character(noSteps.sinceLastCommit + 1), value = as.integer(c(batchNo, k, k + ((batchNo - 1) * runs), combinedStates.total)))
      #data.table::set(x = allStates.table.t, j = as.character(noSteps.sinceLastCommit + 1), value = c(tdiff, t - tdiff))


      # Save the results to disk
      #if (noSteps.sinceLastCommit == (commitInterval - 1)) {
      #  n_saves <- n_steps %/% commitInterval
       # aquanet::commitResults(df_states = allStates.table,
       #                        df_time = allStates.table.t,
       #                        n_states = no.variables,
       #                        n_sites = n_sites,
       #                        site_indices = site_index,
       #                        commit_int = commitInterval,
       #                        iteration_vector = iterationID.vector,
       #                        batch_num = batchNo,
       #                        simulation_num = simNo,
       #                        save_num = n_saves,
       #                        filepath_results = locationSaveResults)
      #  allStates.table[,as.character(iterationID.vector):=empty.vector]
      #  allStates.table.t[,as.character(iterationID.vector):=empty.vector.t]
      #}

      # Pick the next event, and modify a site's state accordingly
      event.objects <- aquanet::doEvent(state_vector = state_vector,
                                       control_matrix = control_matrix,
                                       transition_rates = transition.rates,
                                       tdiff = tdiff,
                                       move_restricted_sites = movement.restrictions.bySite,
                                       non_peak_season = winter,
                                       run_time_params = ListRunTimeParameters,
                                       n_catchments = n_catchments,
                                       spmatrix_sites_catchment = graph.catchment2site.matrix2,
                                       time_vector = time_vector,
                                       catchment_time_vector = catchment_time_vector,
                                       catchments_with_post_fallow_only = catchments.all.sites.c5.status,
                                       source_inf_vector = source.infection.vector,
                                       source_inf_matrix = infected.source.matrix)

      state_vector <- event.objects[[1]]
      control_matrix <- event.objects[[2]]
      time_vector <- event.objects[[3]]
      catchment_time_vector <- event.objects[[4]]
      catchments.all.sites.c5.status <- event.objects[[5]]
      source.infection.vector <- event.objects[[7]]
      rate.type <- event.objects[[8]]
      infected.source.matrix <- event.objects[[9]]

      if (n_steps%%100 == 1) {
        print(c(k,n_steps,length(state_vector),sum(state_vector),tdiff,length(transition.rates[[3]])))
      }
    }
  }

  # Print diagnositic information, and format results as appriopriate
  print(c("No Iterations", n_steps))

  #allStates.table[,as.character((noSteps.sinceLastCommit + 1):commitInterval):=NULL]
  #allStates.table.t[,as.character((noSteps.sinceLastCommit + 1):commitInterval):=NULL]
  #n_saves <- n_saves + 1
  # aquanet::commitResults(df_states = allStates.table,
  #                        df_time = allStates.table.t,
  #                        n_states = no.variables,
  #                        n_sites = n_sites,
  #                        site_indices = site_index,
  #                        commit_int = commitInterval,
  #                        iteration_vector = iterationID.vector,
  #                        batch_num = batchNo,
  #                        simulation_num = simNo,
  #                        save_num = n_saves,
  #                        filepath_results = locationSaveResults)

  save(summaryStates.table,
       file = paste(locationSaveResults,"/Summary/batchNo-", batchNo,".RData", sep = ""),
       compress=FALSE)

  return(batchNo)
}
