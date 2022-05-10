CreateContactPMatrix <- function(graph, periodDataCollection) {
  # Convert the contact network to a matrix, recording the number of movements that occur between sites
  contactp <- igraph::get.adjacency(graph, attr = "movements", names  = TRUE, sparse = TRUE)

  # Divide the number of movements by the time over which information was collected
  contactp.prob <- contactp/periodDataCollection

  # If there are any contacts with a probability of greater than one, assume a probability of one
  contactp.prob[contactp.prob > 1] <- 1

  # Convert the matrix to 'dgTMatrix' type, so it is easier to lookup the source of infection
  contactp.prob <- methods::as(contactp.prob, 'dgTMatrix')

  # Check the final number of sites represented within the model
  LengthContactP <- length(contactp.prob[, 1])

  return(list(LengthContactP, contactp, contactp.prob))
}
