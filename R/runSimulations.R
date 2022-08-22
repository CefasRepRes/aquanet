#' runSimulations
#'
#' If `clear_results` = TRUE, the runSimulations function first clears all .RData files within the
#' `filepath_results` and daughter directories.
#'
#' The number of simulations per job is calculated and a set of R copies running in parallel,
#' communicating over sockets is generated.
#'
#' The `aquanet::simulationCode()` function is then called in parallel to run the aquanet-mod
#' simulation with defined inputs. One complete, the parellel R copies are stopped.
#'
#'
#' @param n_cores (class numeric) the number of cores to use for parallel computing.
#'
#' @param n_sims (class numeric) number of simulations.
#'
#' @param seed_num (class numeric) number used to generate random seed for result replication.
#'
#' @param clear_results (class logical) TRUE/FALSE stating whether .RData results should be cleared
#' from `filepath_results` and nested directories. This ensures if you re-run aquanet-mod the
#' results files are all obtained from a single run.
#'
#' @param tmax (class numeric) maximum amount of time in days that each simulation should run for.
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
#' @param out_createContactProbabilityMatrixTopSitesRemoved (class list) of length 3 containing (1) number of sites
#'  in (live fish) movements matrix (integer), (2) (live fish) movements matrix (dgCMatrix, Matrix
#' package), and (3) probability of (live fish) movements matrix (dgTMatrix, Matrix package).
#' This object is created following the removal of the top most connected sites in the network.
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
#' catchment, "None" means there are no catchment level controls).
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
#' @param filepath_results (class string) path to results directory for model run.
#'
#' @return (class list) of length 2 containing (1) the number of cores used for the run and (2) the
#' output of the foreach loop running the `aquanet::simulationCode()` function.
#'
#' @export
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @import doRNG
runSimulations <- function(n_cores,
                           n_sims,
                           seed_num,
                           clear_results,
                           tmax,
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
                           disease_controls) {

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
  n_sims_per_job <- ceiling(n_sims/ n_cores)

  # create set of copies of R running in parallel communicating over sockets - save output to log file
  cluster <- parallel::makeCluster(n_cores, outfile = "log.txt")

  # register the parallel backend with the foreach package
  doParallel::registerDoParallel(cluster)

  # calculate number of interactions
  n_overall_interactions <- n_sims_per_job * n_cores

  # print number of cores/jobs, simulations per jobs and number of interactions
  print(c(n_cores, n_sims_per_job, n_overall_interactions))

  # set seed
  set.seed(seed_num)

  # run simulation in parallel
  allruns <-
    foreach::foreach(batch_num = 1:n_cores, .combine = c) %dorng% aquanet::simulationCode(
      runs = n_sims_per_job,
      tmax = tmax,
      batch_num = batch_num,
      run_time_params = run_time_params,
      non_peak_season_length = non_peak_season_length,
      out_createContactProbabilityMatrix = out_createContactProbabilityMatrix,
      out_createContactProbabilityMatrixTopSitesRemoved = out_createContactProbabilityMatrixTopSitesRemoved,
      out_createWithinCatchmentEdges = out_createWithinCatchmentEdges,
      out_createCatchmentToSiteMatrix = out_createCatchmentToSiteMatrix,
      out_createRiverDistanceProbabilityMatrix = out_createRiverDistanceProbabilityMatrix,
      out_createDistanceMatrix = out_createDistanceMatrix,
      farm_vector = farm_vector,
      n_states = n_states,
      n_initial_infections = n_initial_infections,
      type_catchment_controls = type_catchment_controls,
      contact_tracing = contact_tracing,
      filepath_results = filepath_results,
      remove_top_sites = remove_top_sites,
      n_infections_remove_top_sites = n_infections_remove_top_sites,
      disease_controls = disease_controls
    )

  n_infections_remove_top_sites# shut down set of copies of R running in parallel communicating over sockets
  parallel::stopCluster(cl = cluster)

  return(list(n_cores, allruns))
}
