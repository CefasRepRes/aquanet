runSimulations <- function(graph.contactp.objects,
                           simulationCode,
                           ListRunTimeParameters,
                           graph.withinCatchmentEdges.objects,
                           graph.catchment2Site.objects,
                           graph.riverDistance.objects,
                           graph.estimateSiteDistances.objects,
                           farm_vector,
                           associatedSiteControlType,
                           noCores,
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

  n_jobs <- noCores
  noSimsPerJob <- ceiling(3000/ n_jobs)

  # Check number of cores available
  parallel::detectCores()

  # Assign 12 cores to the cluster, and save all the output to a log file
  Cluster <- parallel::makeCluster(noCores, outfile = "log.txt")

  # Register cluster
  parallel::registerDoParallel(Cluster)

  overallNoInterations <- noSimsPerJob * n_jobs

  print(c(n_jobs, noSimsPerJob, overallNoInterations))

  set.seed(seedNo)

  allruns <-
    foreach::foreach(batchNo = 1:n_jobs, .combine = c) %dorng% aquanet::simulationCode(
      graph.contactp.objects,
      noSimsPerJob,
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

  parallel::stopCluster(cl = Cluster)

  return(list(n_jobs, allruns))
}
