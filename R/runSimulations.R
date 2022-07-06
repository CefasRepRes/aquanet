runSimulations <- function(graph.contactp.objects,
                           simulationCode,
                           ListRunTimeParameters,
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
                           tmax) {

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

  # Register cluster
  parallel::registerDoParallel(cluster)

  n_overall_interactions <- n_sims_per_job * n_cores

  print(c(n_cores, n_sims_per_job, n_overall_interactions))

  set.seed(seedNo)

  allruns <-
    foreach::foreach(batchNo = 1:n_cores, .combine = c) %dorng% aquanet::simulationCode(
      graph.contactp.objects,
      n_sims_per_job,
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
      n_initial_infections
    )

  parallel::stopCluster(cl = cluster)

  return(list(n_cores, allruns))
}
