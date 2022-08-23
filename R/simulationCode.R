#' simulationCode
#'
#' This function contains the core code to run a simulation of aquanet-mod.
#'
#' In the first segments of the code, values are extracted from input parameters and empty variables
#'  to populate with each simulation are created. The latter includes number of steps, operations,
#' saves, commit intervals and a `site_index`.
#'
#' The for loop:
#'
#' Then, for each run a simulation number is calculated, and various variables are reset including:
#' the time, time difference, transition type, catchment time vector, catchments with only sites in
#' post fallow state, time vector, control matrix, state vector, cumulative site states, infection
#' source vector, matrix of contacts to exclude, catchments controlled in the previous run, and
#' sites under control. A list of catchment movement objects is created. A vector of farms that are
#' able to cull is updated with inclusion of a random number of approximately 50% fisheries assumed
#' to be able to cull.
#'
#' A vector of farms where infection could be seeded is generated and then a farm is chosen at
#' random to seed infection and their infection status updated.
#'
#' The while loop:
#'
#' While the simulation time is below the maximum simulation time `tmax` determine which season it
#' is. Input seasonality information into `aquanet::updateRates()` function and extract the outputs.
#' Calculate a state number for each of the sites in the network and summarise number of sites in
#' each infection/control state.
#'
#' Increment the number of operations and add the summary of states to the summary states table. If
#' the operation is one prior to the commit interval, extend the summary states table in size.
#'
#' If there are no more infected sites within the network, break the while loop.
#'
#' Choose a random weighted exponential distribution value to increment the simulation time by and
#' increase the number of steps.
#'
#' Pick the next transmission event with `aquanet::doEvent()` and modify site's state accordingly.
#' Update variables given outputs.
#'
#' Finally, after the for and while loop, save the summary states table.
#'
#'
#' @param runs (class numeric) number of runs.
#'
#' @param tmax (class numeric) maximum amount of time in days that each simulation should run for.
#'
#' @param batch_num (class numeric) batch number of parallel run.
#'
#' @param run_time_params (class data frame) of model run time parameters imported from original
#' parameter file which is subsequently split into model set up and model run time parameters. Data
#' frame contains probabilities from the scenarios of interest for listing transition rates.
#'
#' @param non_peak_season_length (class string) the amount of time in days that the non peak
#' transmission season lasts for (e.g. "90", "180").
#'
#' @param out_createContactProbabilityMatrix (class list) of length 3 containing (1) number of sites
#'  in (live fish) movements matrix (integer), (2) (live fish) movements matrix (dgCMatrix, Matrix
#' package), and (3) probability of (live fish) movements matrix (dgTMatrix, Matrix package).
#'
#' @param out_createContactProbabilityMatrixTopSitesRemoved (class list) of length 3 containing (1)
#' number of sites in (live fish) movements matrix (integer), (2) (live fish) movements matrix
#' (dgCMatrix, Matrix package), and (3) probability of (live fish) movements matrix (dgTMatrix,
#' Matrix package). This object is created following the removal of the top most connected sites
#' in the network.
#'
#' @param out_createWithinCatchmentEdges (class list) of length 3 containing (1) lgCMatrix (logical
#' matrix) detailing within catchment connections, (2) edge matrix of vertex IDs within catchments,
#' and (3) matrix of source site and receiving site within catchment edges.
#'
#' @param out_createCatchmentToSiteMatrix (class list) of length 2 containing (1) data frame of site
#'  to catchment information and (2) dgCMatrix sparse matrix containing site to catchment summary.
#'
#' @param out_createRiverDistanceProbabilityMatrix (class list) of length 2 containing (1)
#' distances between sites on a river network (via river connectivity) connections of 0 distances
#' are removed (data frame) and (2) sparse matrix containing probability of transmission between
#' sites connected via the river network by river water (dgTMatrix).
#'
#' @param out_createDistanceMatrix (class list) of length 3 containing (1) a matrix of site to site
#' distances (class matrix array), (2) a matrix of distance-based transmission probabilities
#' (dgTMatrix, Matrix package), and (3) input data frame of site catchment locality
#' (SpatialPointsDataFrame, sp package).
#'
#' @param farm_vector (class numeric) numeric binary vector of length number of sites containing
#' information on whether each site 1 = is a farm or 0 = is not a farm.
#'
#' @param n_states (class numeric) number of different combinations of states possible within the
#' model.
#'
#' @param n_initial_infections (class numeric) number of farms to seed infection at.
#'
#' @param type_catchment_controls (class numeric) tnumber selecting catchment level controls to
#' apply (0 = allows movements within the same catchments, 1 = allows movements within or between
#' infected catchments, and 2 = allows no movements by any of the sites within an infected
#' catchment).
#'
#' @param filepath_results (class string) path to results directory for model run.
#'
#' @param contact_tracing (class logical) vector of length 1 indicating whether or not contact
#' tracing is taking place.
#'
#' @param remove_top_sites (class logical) vector of length 1 indicating whether or not the remova
#' of the most connected sites in the network is taking place.
#'
#' @param n_infections_remove_top_sites (class numeric) vector of length 1. After the cumulative
#' number of infected sites exceeds this number, switch to using the top sites removed contact
#' probability matrix.
#'
#' @param disease_controls (class logical) vector of length 1 indicating whether or not
#' any disease control measurs are taking place.
#'
#' @param proportion_cullable (class numeric) proportion of fisheries able to cull site.
#'
#' @return (class numeric) batch number `batch_num` and summaryStates.table saved to
#' `filepath_results`.
#'
#'
#' @export
#'
#' @importFrom methods new
#' @importFrom stats na.omit rexp runif setNames
#' @importFrom data.table data.table :=
#' @importFrom Matrix Matrix
simulationCode <- function(runs,
                           tmax,
                           batch_num,
                           run_time_params,
                           non_peak_season_length,
                           out_createContactProbabilityMatrix,
                           out_createContactProbabilityMatrixTopSitesRemoved,
                           out_createWithinCatchmentEdges,
                           out_createCatchmentToSiteMatrix,
                           out_createRiverDistanceProbabilityMatrix,
                           out_createDistanceMatrix,
                           farm_vector,
                           n_states,
                           n_initial_infections,
                           type_catchment_controls,
                           filepath_results,
                           contact_tracing,
                           remove_top_sites,
                           n_infections_remove_top_sites,
                           disease_controls,
                           proportion_cullable) {

  ## extract information from input parameters ----

  # extract number of sites and matrix of live fish movements contact probabilities
  n_sites <- out_createContactProbabilityMatrix[[1]]
  matrix_movements_prob <- out_createContactProbabilityMatrix[[3]]

  # extract number of catchments and matrix of catchment to site relationships
  spmatrix_sites_catchment <- out_createCatchmentToSiteMatrix[[2]]
  n_catchments <- spmatrix_sites_catchment@Dim[2]

  # extract matrix of contacts between sites within the same catchment
  lgmatrix_catch_catch <- out_createWithinCatchmentEdges[[1]]


  ## create variables to populate ----

  # record number of steps, operations (including premature termination), and full saves per simulation
  n_steps <- 0
  n_operations <- 0
  n_saves <- 0

  # define interval at which results should be saved and create vector of iterations
  commit_int <- 5000
  iteration_vector <- 1:commit_int

  # create vector of 0-based site indices (unique positions within matrix)
  site_index <- 0:(n_sites - 1)

  # create empty result tables to populate (speeds up for loop by memory pre-allocation)
  # TODO: do these need "empty.vector" columns?
  allStates.table <- stats::setNames(data.table::data.table(matrix(0,
                                                                   nrow = 5 + n_sites + n_states, # 3 batch_num, k, sim_num
                                                                   ncol = commit_int)),
                                     as.character(iteration_vector))

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

    # define infection status at sites
    sites_states_cumulative <- state_vector

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
    if(disease_controls == TRUE) {
    # if a site is a fishery get random probability it can be culled
    culling_vector <- ifelse(farm_vector == 1, 0, stats::runif(length(farm_vector)))

    # if site has culling probability below proportion cullable it can be culled (includes farms)
    culling_vector <- ifelse(culling_vector <= proportion_cullable, 1, 0)
    }

    ## randomly select initial site to seed infection (Note: always a farm) ----

    # reset for loop input and farm selection vector
    d <- 0
    farm_select <- c()

    # for each site generate a value and if this is > 0 & not NA add to farm_select vector
    for(d in 0:length(farm_vector)) {
      d <- d + 1
      value <- farm_vector[d] * d
      if (value > 0 & !is.na(value)) {
      farm_select <- c(farm_select, value)
      }
    }

    # select farm to seed from list of start sites
    seed_farm <- sample(farm_select, n_initial_infections)

    # mark this site as infected
    state_vector[seed_farm] <- 1


    ## keep iterating until time reaches maximum allowed ----

    while(t < tmax) {

      # determine which season it is during time point as this impacts transmission dynamics
      non_peak_season <- aquanet::isNonPeakTransmissionSeason(t = t, period = non_peak_season_length)

      # update transition rates, catchment movements, and movement restricted sites
      updated_rates <- aquanet::updateRates(control_matrix = control_matrix,
                                            state_vector = state_vector,
                                            farm_vector =  farm_vector,
                                            culling_vector = culling_vector,
                                            site_indices = site_index,
                                            catchment_movements = list_catchment_movements,
                                            movements_prob = out_createContactProbabilityMatrix,
                                            movements_prob_top_sites_removed = out_createContactProbabilityMatrixTopSitesRemoved,
                                            river_prob = out_createRiverDistanceProbabilityMatrix,
                                            site_distances_prob = out_createDistanceMatrix,
                                            run_time_params = run_time_params,
                                            non_peak_season = non_peak_season,
                                            contact_tracing = contact_tracing,
                                            remove_top_sites = remove_top_sites,
                                            sites_states_cumulative = sites_states_cumulative,
                                            n_infections_remove_top_sites = n_infections_remove_top_sites,
                                            disease_controls = disease_controls)

      # extract list of all transition rates
      transition_rates <- updated_rates[[1]]

      # extract updated list of catchment movement rules - edited by excludeWithinCatchmentMovements() in updateRates()
      list_catchment_movements <- updated_rates[[2]]

      # extract updated logical vector of sites with movement restrictions
      sites_all_movement_restricted <- updated_rates[[3]]

      # extract updated logical vector of sites with controlled status
      sites_controlled <- list_catchment_movements[[6]]

      # update the number of controlled catchments (updated in excludeWithinCatchmentMovements in updateRates)
      n_catchments_controlled <- list_catchment_movements[[7]]

      # calculate site attributes (infection and control status) as a single state per site
      sites_states_vector <- as.integer((state_vector * 10) +
                                          (sites_controlled * 20) +
                                          (control_matrix[ , 2:6] %*% 2:6) +
                                          control_matrix[ , 7])


      # sites: summarise number of sites in each of the n_states ----

      sites_states_totals <- tabulate(sites_states_vector, nbins = n_states)
      sites_states_cumulative <- (state_vector | sites_states_cumulative)


      # increment the number of operations
      n_operations <- n_operations + 1

      # in column n_operations of summaryStates.table append 50 values from time step
      summaryStates.table[ , as.character(n_operations) := c(batch_num,
                                                             k,
                                                             t,
                                                             tdiff,
                                                             sim_num,
                                                             trans_type,
                                                             n_catchments_controlled,
                                                             sum(sites_states_cumulative),
                                                             sites_states_totals)]

      # if the simulation is one step prior to reaching a commit interval
      if (n_operations %% commit_int == (commit_int - 1)) {
        # append another commit_int number of columns of 0 to populate in the next steps
        summaryStates.table[ ,
                             as.character((ncol(summaryStates.table) + 1):(ncol(summaryStates.table) + 1 + commit_int)) :=
                               rep(0, n_states + 8)]
      }

      # if there are no infectious sites in the network stop the loop
      if (length(transition_rates[[3]]) == 0) {
        break()
      }

      # randomly pick next time step, based on a weighted expontial distribution
      tdiff <- stats::rexp(1, sum(transition_rates[[3]]))

      # increment the time and steps
      t <- t + tdiff
      n_steps <- n_steps + 1

      # determine the number of steps since the last commit
      n_steps_since_commit <- n_steps %% commit_int

      # populate allStates.table with simulation information, time and states
      allStates.table[ , as.character(n_steps_since_commit) := c(batch_num,
                                                                 k,
                                                                 sim_num,
                                                                 tdiff,
                                                                 (t - tdiff),
                                                                 sites_states_totals,
                                                                 sites_states_vector)]

      # if the number of steps since last commit equals commit_int - 1
      if (n_steps_since_commit == (commit_int - 1)) {

        # determine number of saves (1 per commit_int)
        n_saves <- n_steps %/% commit_int

        # save the results of allStates.table
        aquanet::commitResults(df_states = allStates.table,
                               n_states = n_states,
                               n_sites = n_sites,
                               site_indices = site_index,
                               commit_int = commit_int,
                               iteration_vector = iteration_vector,
                               batch_num = batch_num,
                               simulation_num = sim_num,
                               save_num = n_saves,
                               filepath_results = filepath_results)

        # clear the tables after commit
        allStates.table[ , as.character(iteration_vector) := rep(0, 5 + n_sites + n_states)]
      }

      # pick the next transmission event and modify a site's state accordingly
      doEvent_out <- aquanet::doEvent(state_vector = state_vector,
                                      control_matrix = control_matrix,
                                      transition_rates = transition_rates,
                                      tdiff = tdiff,
                                      move_restricted_sites = sites_all_movement_restricted,
                                      non_peak_season = non_peak_season,
                                      run_time_params = run_time_params,
                                      n_catchments = n_catchments,
                                      spmatrix_sites_catchment = spmatrix_sites_catchment,
                                      time_vector = time_vector,
                                      catchment_time_vector = catchment_time_vector,
                                      catchments_with_post_fallow_only = catchments_with_post_fallow_only,
                                      source_inf_vector = source_inf_vector,
                                      source_inf_matrix = source_inf_matrix,
                                      contact_tracing = contact_tracing)

      # reassign variables with updates outputs for next iteration of while loop
      state_vector <- doEvent_out[[1]]
      control_matrix <- doEvent_out[[2]]
      time_vector <- doEvent_out[[3]]
      catchment_time_vector <- doEvent_out[[4]]
      catchments_with_post_fallow_only <- doEvent_out[[5]]
      source_inf_vector <- doEvent_out[[6]]
      trans_type <- doEvent_out[[7]]
      source_inf_matrix <- doEvent_out[[8]]

      # every 100 steps print run information to screen #TODO needed?
      if (n_steps %% 100 == 1) {
        print(c(k,
                n_steps,
                length(state_vector),
                sum(state_vector),
                tdiff,
                length(transition_rates[[3]])))
      }
    }
  }


  # where loop terminates between commit intervals: remove empty end of data frame
  allStates.table[ , as.character((n_steps_since_commit + 1):commit_int) := NULL]

  # increment number of saves
  n_saves <- n_saves + 1

  # commit remaining results
  aquanet::commitResults(df_states = allStates.table,
                          n_states = n_states,
                          n_sites = n_sites,
                          site_indices = site_index,
                          commit_int = commit_int,
                          iteration_vector = iteration_vector,
                          batch_num = batch_num,
                          simulation_num = sim_num,
                          save_num = n_saves,
                          filepath_results = filepath_results)

  # save table containing number of sites in each state at each time point
  # TODO update this so the filepath isn't hard-coded once aquanet-mod pr is merged
  save(summaryStates.table,
       file = paste(filepath_results, "/batch_results/batchNo-", batch_num, ".RData", sep = ""),
       compress = FALSE)

  return(batch_num)
}
