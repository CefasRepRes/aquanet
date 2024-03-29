#' createFarmVector
#'
#' Use the connectivity matrix as input to return a numeric binary vector indicating site attributes
#'
#' @param graph (class igraph) graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-mod. This includes both live fish movements and
#' Section 30 movements.
#'
#' @return (class numeric) vector of length 'number of sites in the connectivity matrix' containing
#' 1's depicting sites that are of type "Farm" and 0's for all other site types.
#'
#' @export
#'
#' @importFrom igraph V
createFarmVector <- function(graph) {
  # extract site type from sites in connectivity matrix
  site_type <- igraph::V(graph)$type

  # if site type is "Farm" return 1 (else zero)
  farm_vector <- as.numeric(site_type == "Farm")

  # return binary (1/0) vector indicating farms in all sites
  return(farm_vector)
}
