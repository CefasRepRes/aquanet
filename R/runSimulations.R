runSimulations <- function(n_cores,
                           seedNo,
                           clear_results,
                           tmax,
                           run_time_params,
                           non_peak_season_length,
                           out_createContactProbabilityMatrix,
                           out_createWithinCatchmentEdges,
                           out_createCatchmentToSiteMatrix,
                           out_list_createRiverDistanceProbabilityMatrix,
                           out_createDistanceMatrix,
                           farm_vector,
                           n_states,
                           n_initial_infections,
                           type_catchment_controls,
                           filepath_results,
                           simulationCode) {

  if (clear_results == TRUE) {
  # list files ending in .RData in the results directory
  files <- list.files(path = filepath_results,
                      pattern = "\\.RData$",
                      recursive = TRUE,
                      full.names = TRUE)

  # delete .RData files present to ensure results are from one run
  do.call(file.remove, list(files))
  }

  # define number of simulations per job (n_jobs == n_cores)
  n_sims_per_job <- ceiling(3000/ n_cores)

  # create set of copies of R running in parallel communicating over sockets - save output to log file
  cluster <- parallel::makeCluster(n_cores, outfile = "log.txt")

  # register the parallel backend with the foreach package
  parallel::registerDoParallel(cluster)

  # calculate number of interactions
  n_overall_interactions <- n_sims_per_job * n_cores

  # print number of cores/jobs, simulations per jobs and number of interactions
  print(c(n_cores, n_sims_per_job, n_overall_interactions))

  # set seed
  set.seed(seedNo)

  # run simulation in parallel
  allruns <-
    foreach::foreach(batchNo = 1:n_cores, .combine = c) %dorng% aquanet::simulationCode(
      runs = n_sims_per_job,
      tmax = tmax,
      batch_num = batchNo,
      run_time_params = run_time_params,
      non_peak_season_length = non_peak_season_length,
      out_createContactProbabilityMatrix = out_createContactProbabilityMatrix,
      out_createWithinCatchmentEdges = out_createWithinCatchmentEdges,
      out_createCatchmentToSiteMatrix = out_createCatchmentToSiteMatrix,
      out_list_createRiverDistanceProbabilityMatrix = out_list_createRiverDistanceProbabilityMatrix,
      out_createDistanceMatrix = out_createDistanceMatrix,
      farm_vector = farm_vector,
      n_states = n_states,
      n_initial_infections = n_initial_infections,
      type_catchment_controls = type_catchment_controls,
      filepath_results = filepath_results
    )

  # shut down set of copies of R running in parallel communicating over sockets
  parallel::stopCluster(cl = cluster)

  return(list(n_cores, allruns))
}
