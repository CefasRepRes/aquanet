CreateFarmVector <- function(graph) {
  # extract site type from sites in connectivity matrix
  site_type <- igraph::V(graph)$type

  # if site type is "Farm" return 1 (else zero)
  farm_vector <- as.numeric(site_type == "Farm")

  # return binary (1/0) vector indicating farms in all sites
  return(farm_vector)
}
