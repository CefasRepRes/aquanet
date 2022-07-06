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

  contactp.length <- graph.contactp.objects[[1]]

  noJobs <- noCores
  noSimsPerJob <- ceiling(3000/ noJobs)

  # Check number of cores available
  detectCores()

  # Assign 12 cores to the cluster, and save all the output to a log file
  Cluster <- makeCluster(noCores, outfile = "log.txt")

  # Register cluster
  registerDoParallel(Cluster)

  overallNoInterations <- noSimsPerJob * noJobs

  print(c(noJobs, noSimsPerJob, overallNoInterations))

  set.seed(seedNo)
  allruns <- foreach(batchNo=1:noJobs, .combine=c) %dorng% simulationCode(graph.contactp.objects, noSimsPerJob, tmax,batchNo, ListRunTimeParameters, graph.withinCatchmentEdges.objects, graph.catchment2Site.objects, graph.riverDistance.objects, graph.estimateSiteDistances.objects, farm_vector, associatedSiteControlType, locationSaveResults, n_initial_infections)

  stopCluster(cl = Cluster)

  return(list(noJobs, allruns))
}
