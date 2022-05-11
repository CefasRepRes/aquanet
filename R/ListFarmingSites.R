CreateFarmVector <- function(graph) {
  graph.siteType <- igraph::V(graph)$type
  farm_vector <- as.numeric(graph.siteType == "Farm")

  return(farm_vector)
}
