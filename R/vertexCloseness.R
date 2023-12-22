#' vertexCloseness
#' Summary of vertex closeness of contact network, which ranks which nodes (susceptible contacts) are significant,
#' based on the length of the shortest paths between a node to all other nodes in the network.
#' @param contact_network (class igraph)  Graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-mod.
#' @return network_closeness
#' @export
#'
#'
vertexCloseness <- function(contact_network){
  # get closeness of contact network
  closen <- igraph::closeness(graph = contact_network,
                              vids = V(contact_network),
                              mode = "all")
  # summarise
  network_closeness <- data.frame(mean = mean(closen),
                                  median = median(closen),
                                  min = min(closen),
                                  max = max(closen))
  return(network_closeness)
}
