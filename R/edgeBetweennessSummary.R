#' Edge Betweenness summary
#' Summary of edge betweenness of contact network, which ranks which links are significant, based on the fraction of shortest paths that pass through the links between network nodes (susceptible contacts).
#' @return network_betweenness
#' @param contact_network network simulates and tracks disease transmission from infected to susceptible contacts
#' @return network_betweenness
#' @export
#'
#'
edgeBetweennessSummary <- function(contact_network){
  between <- igraph::edge.betweenness(graph = contact_network,
                                      e = E(contact_network))
  network_betweenness <- data.frame(mean = mean(between),
                                    median = median(between),
                                    min = min(between),
                                    max = max(between))
  return(network_betweenness)
}

