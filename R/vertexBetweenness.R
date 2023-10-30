#' Vertex Betweenness summary statistics
#' Vertex Betweenness summary statistics. Indicating the measure of centrality in a graph based on shortest paths.
#' @param contact_network network simulates and tracks disease transmission from infected to susceptible contacts
#'
#' @return network_betweenness
#' @export

vertexBetweenness <- function(contact_network){
  between <- igraph::betweenness(graph = contact_network,
                                 v = V(contact_network))
  network_betweenness <- data.frame(mean = mean(between),
                                    median = median(between),
                                    min = min(between),
                                    max = max(between))
  return(network_betweenness)
}
