#' createContactProbabilityMatrix
#'
#' This function creates a sparse matrix of contact probabilities between sites for disease
#' transmission via the LFM route of AquaNet-Mod (see details).
#'
#' Use the connectivity matrix (`graph`) to extract a movement matrix. Calculate the probability of
#' movement/contact between sites by dividing the number of movements by dividing by the period of
#' time (in days) for which movement information was used (`movement_period`). Then, determine the
#' number of sites present in the contact probability matrix. Return these three values as a list.
#'
#' @param graph (class igraph) Graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-mod. This includes both live fish movements and
#' Section 30 movements.
#'
#' @param movement_period (class numeric) The period of time (in days) for which movement data (live
#'  fish movements and section 30 movements) were collected and used to create `graph` parameter.
#'
#' @return (class list) of length 3 containing:
#' 1. (class integer) number of sites in movements matrix.
#' 2. (class dgCMatrix, Matrix package) movements matrix.
#' 3. (class dgTMatrix, Matrix package) probability of movements matrix.
#'
#' @export
#'
#' @importFrom igraph get.adjacency
#' @importFrom methods as
createContactProbabilityMatrix <- function(graph, movement_period) {
  # get sparse adjacency matrix of movements between sites from contact network (graph)
  matrix_movements <- igraph::get.adjacency(graph,
                                            attr = "movements",
                                            names  = TRUE,
                                            sparse = TRUE)

  # divide number of movements by the time over which movement information was collected
  matrix_movements_prob <- matrix_movements/movement_period

  # if there are any contacts with a probability of greater than one, assume a probability of one
  matrix_movements_prob[matrix_movements_prob > 1] <- 1

  # uncompress 'dgCMatrix' to 'dgTMatrix' type (easier to look up the source of infection)
  matrix_movements_prob <- methods::as(matrix_movements_prob, 'dgTMatrix')

  # extract number of sites represented within the model
  n_sites <- length(matrix_movements_prob[ , 1])

  # return list containing number of sites in contact matrix, adjacency matrix of movements between
  # sites, and probability matrix of movements between sites
  return(list(n_sites = n_sites,
              matrix_movements = matrix_movements,
              matrix_movements_prob = matrix_movements_prob))
}

#' createContactProbabilityMatrixTopSitesRemoved
#'
#' This function creates a sparse matrix of contact probabilities between sites (with top connected
#' sites removed) for disease transmission via the LFM route of AquaNet-Mod (see details).
#'
#' Use the connectivity matrix (`graph`) to extract a movement matrix. Calculate the probability of
#' movement/contact between sites by dividing the number of movements by dividing by the period of
#' time (in days) for which movement information was used (`movement_period`). Extract sites with
#' greater than a user-specified quantile (`percentile`) of in (receiving) movements and out
#' (supplying) movements and overwrite their contact probabilities to zero to negate the impact of
#' top sites in the contact network. Then, determine the number of sites present in the contact
#' probability matrix. Return these three values as a list.
#'
#' @param graph (class igraph) Graph of connections/movements between sites produced with iGraph
#' (using script importSiteData.R of AquaNet-Mod). This includes both live fish movements and
#' Section 30 movements.
#'
#' @param movement_period (class numeric) The period of time (in days) for which movement data (live
#'  fish movements and section 30 movements) were collected and used to create `graph` parameter.
#'
#' @param n_remove (class numeric) The number of topmost connected sites to be removed from the
#' contact probability matrix. This includes sites with highest number of outward (supplying)
#' movements and sites with number of inward (receiving) movements.
#'
#' @return (class list) of length 3 containing:
#' 1. (class integer) number of sites in movements matrix.
#' 2. (class dgCMatrix, Matrix package) movements matrix.
#' 3. (class dgTMatrix, Matrix package) probability of movements matrix with top sites zeroed.
#'
#' @export
#'
#' @import Matrix
#'
#' @importFrom methods as
#' @importFrom igraph get.adjacency
#' @importFrom stats quantile
#' @importFrom dplyr slice_max
#' @importFrom magrittr %>%
createContactProbabilityMatrixTopSitesRemoved <- function(graph,
                                                          movement_period,
                                                          n_remove) {
  # get adjacency matrix of movements between sites from contact network (graph)
  matrix_movements <- igraph::get.adjacency(graph,
                                            attr = "movements",
                                            names = TRUE,
                                            sparse = TRUE)

  # divide number of movements by the time over which movement information was collected
  matrix_movements_prob <- matrix_movements/movement_period

  # if there are any contacts with a probability of greater than one, assume a probability of one
  matrix_movements_prob[matrix_movements_prob > 1] <- 1

  # extract number of sites represented within the model
  n_sites <- length(matrix_movements_prob[ , 1])

  # calculate number of inward and outward movements per a site
  out_per_site <- Matrix::rowSums(matrix_movements)
  in_per_site <- Matrix::colSums(matrix_movements)

  # extract sites whose movements are greater than the percentile-th quantile
  out_sites_remove <- as.data.frame(out_per_site) %>%
    dplyr::slice_max(out_per_site, n = n_remove)
  in_sites_remove <- as.data.frame(in_per_site) %>%
    dplyr::slice_max(in_per_site, n = n_remove)

  # zero any contacts which originate from sites within the percentile-th quantile
  matrix_movements_prob[rownames(out_sites_remove), ] <- 0
  matrix_movements_prob[rownames(in_sites_remove), ] <- 0

  # uncompress 'dgCMatrix' to 'dgTMatrix' type (easier to look up the source of infection)
  matrix_movements_prob <- methods::as(matrix_movements_prob, 'dgTMatrix')

  return(list(n_sites = n_sites,
              matrix_movements = matrix_movements,
              matrix_movements_prob = matrix_movements_prob))
}
