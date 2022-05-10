#' createContactProbabilityMatrix
#'
#' Use the connectivity matrix (`graph`) to extract a movement matrix. Calculate the probability of movement/contact between sites by dividing the number of movements by dividing by the period of time (in days) for which movement information was used (`movement_period`). Then, determine the number of sites present in the contact probability matrix. Return these three values as a list.
#'
#' @param graph (class igraph) Graph of connections/movements between sites produced with iGraph (using script importSiteData.R of AquaNet-Mod). This includes both live fish movements and Section 30 movements.
#' @param movement_period (class numeric) The period of time (in days) for which movement data (live fish movements and section 30 movements) were collected and used to create `graph` parameter.
#'
#' @return (class list) of length 3 containing (1) number of sites in movements matrix (integer), (2) movements matrix (dgCMatrix, Matrix package), and (3) probability of movements matrix (dgTMatrix, Matrix package).
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
  n_sites <- length(matrix_movements_prob[, 1])

  # return list containing number of sites in contact matrix, adjacency matrix of movements between sites,
  # and probability matrix of movements between sites
  return(list(n_sites, matrix_movements, matrix_movements_prob))
}

CreateAltContactPMatrix <- function(graph, movement_period) {
  # get adjacency matrix of movements between sites from contact network (graph)
  matrix_movements <- igraph::get.adjacency(graph, attr = "movements", names = TRUE)

  # divide number of movements by the time over which movement information was collected
  matrix_movements_prob <- matrix_movements/movement_period
  ######## This is to initiate national standstill - i.e. no movements will be allowed if there are certain number of infection in the system
  #matrix_movements_prob <- matrix_movements/movement_period*0

  # if there are any contacts with a probability of greater than one, assume a probability of one
  matrix_movements_prob[matrix_movements_prob > 1] <- 1

  # extract number of sites represented within the model
  n_sites <- length(matrix_movements_prob[, 1])

  # calculate number of inward and outward movements per a site
  out_per_site <- rowSums(matrix_movements)
  in_per_site <- colSums(matrix_movements)

  # extract sites whose movements are greater than the 95th quantile
  out_sites_quantile = names(out_per_site)[out_per_site > quantile(out_per_site, .992)]
  in_sites_quantile <- names(in_per_site)[in_per_site > quantile(in_per_site, .992)]

  # zero any contacts which originate from sites within the 95th quantile
  matrix_movements_prob[out_sites_quantile, ] <- 0
  matrix_movements_prob[in_sites_quantile, ] <- 0

  return(list(n_sites, movement_period, matrix_movements_prob))
}
