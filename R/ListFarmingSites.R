CreateFarmVector = function(graph_full) {
  graph.siteType = V(graph_full)$type
  farm_vector = as.numeric(graph.siteType == "Farm")

  return(farm_vector)
}
