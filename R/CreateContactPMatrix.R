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
