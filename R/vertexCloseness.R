#' Vertex Closeness summary
#' Summary statistics of vertex closeness centrality for the contact network.The closeness centrality of a vertex is the inverse of the sum of the distances from a vertex (point in a graph) to all the other vertices.

#' @param contact_network network simulates and tracks disease transmission from infected to susceptible contacts
#' @return network_closeness
#' @export
#'

vertexCloseness <- function(contact_network){
  closen <- igraph::closeness(graph = contact_network,
                              vids = V(contact_network),
                              mode = "all")
  network_closeness <- data.frame(mean = mean(closen),
                                  median = median(closen),
                                  min = min(closen),
                                  max = max(closen))
  return(network_closeness)
}
