#' simulationCode
#'
#' @param createContactProbabilityMatrix_out TODO (was graph.contactp.objects)
#'
#' @param runs TODO
#'
#' @param tmax TODO
#'
#' @param batch_num TODO (was batchNo)
#'
#' @param run_time_params TODO
#'
#' @param createWithinCatchmentEdges_out TODO (was graph.withinCatchmentEdges.objects)
#'
#' @param createCatchmentToSiteMatrix_out TODO
#'
#' @param createRiverDistanceProbabilityMatrix_out_list TODO
#'
#' @param createDistanceMatrix_out TODO (was graph.estimateSiteDistances.objects)
#'
#' @param farm_vector TODO
#'
#' @param type_catchment_controls TODO
#'
#' @param filepath_results TODO
#'
#' @return
#'
#' @export
#'
#' @importFrom methods new
#' @importFrom stats na.omit rexp runif setNames
#' @importFrom data.table set data.table :=
#' @importFrom Matrix Matrix
simulationCode <- function(createContactProbabilityMatrix_out,
                           runs,
                           tmax,
                           batch_num,
                           run_time_params,
                           non_peak_season,
                           createWithinCatchmentEdges_out,
                           createCatchmentToSiteMatrix_out,
                           createRiverDistanceProbabilityMatrix_out_list,
                           createDistanceMatrix_out,
                           farm_vector,
                           type_catchment_controls,
                           filepath_results) {

  ## extract information from input parameters ----

  # extract number of sites and matrix of live fish movements contact probabilities
  n_sites <- createContactProbabilityMatrix_out[[1]]
  matrix_movements_prob <- createContactProbabilityMatrix_out[[3]]

  # extract number of catchments and matrix of catchment to site relationships
  spmatrix_sites_catchment <- createCatchmentToSiteMatrix_out[[2]]
  n_catchments <- spmatrix_sites_catchment@Dim[2]

  # extract matrix of contacts between sites within the same catchment
  lgmatrix_catch_catch <- createWithinCatchmentEdges_out[[1]]


  ## create variables to populate ----

  # record number of steps, operations (including premature termination), and full saves per simulation
  n_steps <- 0
  n_operations <- 0
  n_saves <- 0

  # define number of possible states within the model
  n_states <- 42

  # define interval at which results should be saved and create vector of iterations
  commit_int <- 5000
  iteration_vector <- 1:commit_int

  # create vector of 0-based site indices (unique positions within matrix)
  site_index <- 0:(n_sites - 1)

  # create empty result tables to populate (speeds up for loop by memory pre-allocation)
  allStates.table <- stats::setNames(data.table::data.table(matrix(0,
                                                                   nrow = n_sites + n_states,
                                                                   ncol = commit_int + 1)),
                                     c("empty.vector", as.character(iteration_vector)))

  allStates.table.t <- stats::setNames(data.table::data.table(matrix(0,
                                                                     nrow = 2,
                                                                     ncol = commit_int + 1)),
                                       c("empty.vector.t", as.character(iteration_vector)))

  summaryStates.table <- stats::setNames(data.table::data.table(matrix(0,
                                                                       nrow = n_states + 8,
                                                                       ncol = commit_int)),
                                         as.character(iteration_vector))


  ## simulate each model run ----

  for (k in 1:runs) {

    # calculate simulation number (equivalent to k and valid across every parallel thread)
    sim_num <- k + ((batch_num - 1) * runs)

    # reset the time, time step, and rate type for the run
    t <- 0
    tdiff <- 0
    trans_type <- 0


    ## create empty variables to record run data ----

    # vector to record time since last status change of catchment
    catchment_time_vector <- rep(0, length = n_catchments)

    # vector to indicate catchments ready to be restocked
    catchments_with_post_fallow_only <- rep(0, length = n_catchments)

    # vector to record time sites have been in state of infection or surveillance/controls
    time_vector <- rep(0, n_sites)

    # matrix to record control measures (columns) in place at each site (rows)
    # TODO: replace column numbers with names & fix other functions with control_matrix input
    control_matrix <- matrix(data = 0, nrow = n_sites, ncol = 7)

    # vector (0/1) to record infection status at sites
    state_vector <- rep(0, n_sites)

    # TODO: check which of these are needed
    cumulativeState_vector <- state_vector
    farmcumulativeState_vector <- state_vector * farm_vector
    fisherycumulativeState_vector <- state_vector * as.numeric(!farm_vector)


    # vector to record source sites responsible for infection via Live Fish Movements/river network
    source_inf_vector <- rep(0, n_sites)

    # matrix to forward trace sites becoming infected (connected via live fish movements/river network)
    source_inf_matrix <- matrix(data = 0, nrow = n_sites, ncol = n_sites)

    # matrix to record contacts affected by catchment-level restrictions in previous time step
    matrix_contacts_exclude  <- methods::new(Class = "dgTMatrix", Dim = c(n_sites, n_sites))

    # matrix to record catchments controlled in the previous time step (avoids recalculation)
    catchments_controlled_prev <- as(Matrix::Matrix(nrow = n_catchments, ncol = 1,
                                                    data = 0, sparse = T), "dgeMatrix")

    # vector to record sites controlled in the previous time step (avoids recalculation)
    sites_controlled <- vector(mode = "logical", length = n_sites)

    # reset number of controlled catchments
    n_catchments_controlled <- 0

    # create list of catchment_movement objects
    list_catchment_movements <- list(spmatrix_sites_catchment,
                                     lgmatrix_catch_catch,
                                     catchments_controlled_prev,
                                     matrix_contacts_exclude,
                                     type_catchment_controls,
                                     sites_controlled,
                                     n_catchments_controlled)


    ## add fisheries that can be culled to culling vector of farms ----

    # if a site is a fishery get random probability it can be culled
    culling_vector <- ifelse(farm_vector == 1, 0, stats::runif(length(farm_vector)))

    # if site has culling probability below 0.5 it can be culled (includes farms)
    culling_vector <- ifelse(culling_vector < 0.5, 1, 0)


    ## randomly select initial site to seed infection (Note: always a farm) ----

    # reset for loop input and farm selection vector
    d <- 0
    farm_select <- c()

    # for each site generate a value and if this is > 0 & not NA add to farm_select vector
    for(d in 0:length(farm_vector)) {
      d <- d + 1
      value <- farm_vector[d] * d
      if (value > 0 & !stats::is.na(value)) {
      farm_select <- c(farm_select, value)
      }
    }

    # select farm to seed from list of start sites
    seed_farm <- sample(farm_select, 1)

    # mark this site as infected
    state_vector[seed_farm] <- 1


    ## keep iterating until time reaches maximum allowed ----

    while(t < tmax) {

      # update transition rates, catchment movements, and movement restricted sites
      update_rate.output.objects <- aquanet::updateRates(control_matrix = control_matrix,
                                                        state_vector = state_vector,
                                                        farm_vector =  farm_vector,
                                                        culling_vector = culling_vector,
                                                        site_indices = site_index,
                                                        catchment_movements = list_catchment_movements,
                                                        movements_prob = createContactProbabilityMatrix_out,
                                                        river_prob = createRiverDistanceProbabilityMatrix_out_list,
                                                        site_distances_prob = createDistanceMatrix_out,
                                                        run_time_params = run_time_params,
                                                        non_peak_season = non_peak_season)

      # extract list of all transition rates
      transition.rates <- update_rate.output.objects[[1]]

      # Cache any calculations on movements out of controlled catchments, to avoid unnecesary recalculation
      list_catchment_movements <- update_rate.output.objects[[2]]
      sites_controlled <- list_catchment_movements[[6]]

      # Retrieve logical vectors for each type of controlled state, to avoid recalculation
      movement.restrictions.bySite <- update_rate.output.objects[[3]]

      # Combine all of the site's attributes into a single state, count the total number of sites per state
      combinedStates_vector <- as.integer((state_vector * 10) +
                                            (sites_controlled * 20) +
                                            (control_matrix[ , 2:6] %*% 2:6) +
                                            control_matrix[ , 7])
      cumulativeState_vector <- (state_vector | cumulativeState_vector)
      farmStates.vector <- farm_vector * state_vector
      farmcumulativeState_vector <- (farmStates.vector | farmcumulativeState_vector)
      fisheriesStates.vector <- state_vector * as.numeric(!farm_vector)
      fisherycumulativeState_vector <- (fisheriesStates.vector | fisherycumulativeState_vector)

      combfarm.vector <- farm_vector * combinedStates_vector
      comfishery.vector <- as.numeric(!farm_vector) * combinedStates_vector
      combinedStates.total <- tabulate(combinedStates_vector, nbins = n_states)
      farmcombinedstates.total <- tabulate(combfarm.vector, nbins = n_states)
      fisheriescombinedstates.total <- tabulate(comfishery.vector, nbins = n_states)

      n_operations <- n_operations + 1
      n_catchments_controlled <- list_catchment_movements[[7]]
      data.table::set(x = summaryStates.table,
                      j = as.character(n_operations),
                      value = c(batch_num,k, t, tdiff, sim_num, trans_type, n_catchments_controlled, sum(farmcumulativeState_vector), combinedStates.total))

      if (n_operations %% commit_int == (commit_int - 1)) {
        summaryStates.table[ , as.character((ncol(summaryStates.table) + 1):(ncol(summaryStates.table) + 1 + commit_int)) := rep(0, n_states + 8)]
      }

      # If there are no infectious sites on the network stop the simulation
      if (length(transition.rates[[3]]) == 0) {
        break()
      }

      # Randomly pick next time step, based on a weighted expontial distribution
      tdiff <- stats::rexp(1, sum(transition.rates[[3]]))

      t <- t + tdiff
      noSteps.sinceLastCommit <- n_steps %% commit_int
      n_steps <- n_steps + 1

      # Record the current state of the network, for analysis over all time periods and simulations
      # Make sure that all of the variables stored in the 'allStates.table' are integers (hence the [0-9]L syntax)
      # For analysis outside of the model, treating a site's state as multidimensional is going to be really confusing
      # The following line of code should combine all the site's attributes into a single number,
      # which uniquely represents all of the attributes co-occuring within the same site

      #data.table::set(x = allStates.table, i = (n_states + 1):(n_states + n_sites),j = as.character(noSteps.sinceLastCommit + 1), value = as.integer(combinedStates_vector))
      #data.table::set(x = allStates.table, i = (1:(n_states + 3)), j = as.character(noSteps.sinceLastCommit + 1), value = as.integer(c(batch_num, k, k + ((batch_num - 1) * runs), combinedStates.total)))
      #data.table::set(x = allStates.table.t, j = as.character(noSteps.sinceLastCommit + 1), value = c(tdiff, t - tdiff))


      # Save the results to disk
      #if (noSteps.sinceLastCommit == (commit_int - 1)) {
      #  n_saves <- n_steps %/% commit_int
       # aquanet::commitResults(df_states = allStates.table,
       #                        df_time = allStates.table.t,
       #                        n_states = n_states,
       #                        n_sites = n_sites,
       #                        site_indices = site_index,
       #                        commit_int = commit_int,
       #                        iteration_vector = iteration_vector,
       #                        batch_num = batch_num,
       #                        simulation_num = sim_num,
       #                        save_num = n_saves,
       #                        filepath_results = filepath_results)
      #  allStates.table[,as.character(iteration_vector):= rep(0, n_sites + n_states)]
      #  allStates.table.t[,as.character(iteration_vector):= rep(0, 2)]
      #}

      # Pick the next event, and modify a site's state accordingly
      event.objects <- aquanet::doEvent(state_vector = state_vector,
                                       control_matrix = control_matrix,
                                       transition_rates = transition.rates,
                                       tdiff = tdiff,
                                       move_restricted_sites = movement.restrictions.bySite,
                                       non_peak_season = non_peak_season,
                                       run_time_params = run_time_params,
                                       n_catchments = n_catchments,
                                       spmatrix_sites_catchment = spmatrix_sites_catchment,
                                       time_vector = time_vector,
                                       catchment_time_vector = catchment_time_vector,
                                       catchments_with_post_fallow_only = catchments_with_post_fallow_only,
                                       source_inf_vector = source_inf_vector,
                                       source_inf_matrix = source_inf_matrix)

      state_vector <- event.objects[[1]]
      control_matrix <- event.objects[[2]]
      time_vector <- event.objects[[3]]
      catchment_time_vector <- event.objects[[4]]
      catchments_with_post_fallow_only <- event.objects[[5]]
      source_inf_vector <- event.objects[[7]]
      trans_type <- event.objects[[8]]
      source_inf_matrix <- event.objects[[9]]

      if (n_steps%%100 == 1) {
        print(c(k,n_steps,length(state_vector),sum(state_vector),tdiff,length(transition.rates[[3]])))
      }
    }
  }

  # Print diagnositic information, and format results as appriopriate
  print(c("No Iterations", n_steps))

  #allStates.table[,as.character((noSteps.sinceLastCommit + 1):commit_int):=NULL]
  #allStates.table.t[,as.character((noSteps.sinceLastCommit + 1):commit_int):=NULL]
  #n_saves <- n_saves + 1
  # aquanet::commitResults(df_states = allStates.table,
  #                        df_time = allStates.table.t,
  #                        n_states = n_states,
  #                        n_sites = n_sites,
  #                        site_indices = site_index,
  #                        commit_int = commit_int,
  #                        iteration_vector = iteration_vector,
  #                        batch_num = batch_num,
  #                        simulation_num = sim_num,
  #                        save_num = n_saves,
  #                        filepath_results = filepath_results)

  save(summaryStates.table,
       file = paste(filepath_results,"/Summary/batchNo-", batch_num,".RData", sep = ""),
       compress=FALSE)

  return(batch_num)
}
