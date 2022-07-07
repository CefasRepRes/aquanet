runSimulations <- function(graph.contactp.objects,
                           simulationCode,
                           run_time_params,
                           graph.withinCatchmentEdges.objects,
                           graph.catchment2Site.objects,
                           graph.riverDistance.objects,
                           graph.estimateSiteDistances.objects,
                           farm_vector,
                           associatedSiteControlType,
                           n_cores,
                           locationSaveResults,
                           seedNo,
                           n_initial_infections,
                           tmax,
                           n_states,
                           non_peak_season_length) {

  # list files ending in .RData in the results directory
  files <- list.files(path = locationSaveResults,
                      pattern = "\\.RData$",
                      recursive = TRUE,
                      full.names = TRUE)

  # delete .RData files present to ensure results are from one run
  do.call(file.remove, list(files))

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
      createContactProbabilityMatrix_out = graph.contactp.objects,
      createWithinCatchmentEdges_out = graph.withinCatchmentEdges.objects,
      createCatchmentToSiteMatrix_out = graph.catchment2Site.objects,
      createRiverDistanceProbabilityMatrix_out_list = graph.riverDistance.objects,
      createDistanceMatrix_out = graph.estimateSiteDistances.objects,
      farm_vector = farm_vector,
      n_states = n_states,
      n_initial_infections = n_initial_infections,
      type_catchment_controls = associatedSiteControlType,
      filepath_results = locationSaveResults
    )

  # shut down set of copies of R running in parallel communicating over sockets
  parallel::stopCluster(cl = cluster)

  return(list(n_cores, allruns))
}
