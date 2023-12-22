#' edgeBetweennessSummary
#' Summary of edge betweenness of contact network, which ranks which links are significant,
#' based on the fraction of shortest paths that pass through the links between network
#' nodes (susceptible contacts).
#' @return network_betweenness
#' @param contact_network (class igraph)  Graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-mod.
#' @return network_betweenness
#' @export
#'
#'
edgeBetweennessSummary <- function(contact_network){

  # get betweenness of contact network
  between <- igraph::edge.betweenness(graph = contact_network,
                                      e = igraph::E(contact_network))

  # summarise
  network_betweenness <- data.frame(mean = mean(between),
                                    median = median(between),
                                    min = min(between),
                                    max = max(between))
  return(network_betweenness)
}

