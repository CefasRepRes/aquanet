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

CreateAltContactPMatrix <- function(graph, movement_period,metricType) {

  # Convert the contact network to a matrix, recording the number of movements that occur between sites
  matrix_movements <- igraph::get.adjacency(graph, attr="movements", names=TRUE)

  # Divide the number of movements by the time over which information was collected
  contactp.prob <- matrix_movements/movement_period
  ######## This is to initiate national standstill - i.e. no movements will be allowed if there are certain number of infection in the system
  #contactp.prob=matrix_movements/movement_period*0

  # If there are any contacts with a probability of greater than one, assume a probability of one
  contactp.prob[contactp.prob>1] <- 1

  # Check the final number of sites represented within the model
  LengthContactP <- length(contactp.prob[,1])

  # Calculate no. of outward movements per a site
  outwardMovementsPerSite <- rowSums(matrix_movements)
  # Calculate the number of inwared movements per site
  inwardMovementsPerSite <- rowSums(t(matrix_movements))


  # Extract a list of sites whose movements are greater than the 95th quantile
  sitesGreaterQuantile <- names(outwardMovementsPerSite)[outwardMovementsPerSite > quantile(outwardMovementsPerSite,.992)]
  InsitesGreaterQuantile <- names(inwardMovementsPerSite)[inwardMovementsPerSite > quantile(inwardMovementsPerSite,.992)]

  # Zero any contacts which originate from sites within the 95th quantile
  contactp.prob[sitesGreaterQuantile,] <- 0
  contactp.prob[InsitesGreaterQuantile,] <- 0

  return(list(LengthContactP, movement_period, contactp.prob))
}
