#' vertexBetweenness
#' Summary of vertex betweenness of contact network, which ranks which nodes (susceptible contacts) are significant,
#' based on the fraction of shortest paths that pass through each node.
#' @param contact_network (class igraph)  Graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-mod.
#' @return network_betweenness
#' @export
#'

vertexBetweenness <- function(contact_network){

  # get betweenness of Contact network
  between <- igraph::betweenness(graph = contact_network,
                                 v = V(contact_network))

  # summarise
  network_betweenness <- data.frame(mean = mean(between),
                                    median = median(between),
                                    min = min(between),
                                    max = max(between))
  return(network_betweenness)
}
