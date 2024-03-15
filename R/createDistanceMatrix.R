#' createDistanceMatrix
#'
#' This function creates a sparse matrix for distance-related transmission probabilities between
#' sites for disease transmission via SDM route of AquaNet-Mod (see details).
#'
#' Using the connectivity igraph and site to catchment information .csv create a matrix of distances
#'  between these sites calculated using the sf package and user input coordinate reference system.
#' Additionally, filter these distances to remove self loops and distances greater than
#' sdm_max_dist m (assume greater than sdm_max_dist m constitutes negligible transmission risk).
#' Convert distances to a probability of transmission with the following equation
#'  `sdm_rate_gamma * exp(-(distance)^2 * sdm_scalar_lambda)`. Convert probabilities that are less
#' than 0 or equal to sdm_rate_gamma to 0. Return a list containing site-site distance matrix,
#' site-site distance-based transmission probability matrix and site-catchment data frame. Note:
#' matrices produced have the same reorder distance matrix, so that it is in the same site order
#' as the contact matrix (graph).
#'
#' @param graph (class igraph) graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-mod.
#'
#' @param filename_site_catchments  (class string) string containing the file path and file name for
#'  .csv containing information about site location (easting and northing) and which catchment each
#'  site resides in.
#'
#' @param crs_epsg (class numeric) 4-5 digit epsg code stating the coordinate reference system (crs)
#'  to use for projecting the data.
#'
#' @param sdm_rate_gamma (class numeric) daily short distance mechanical (SDM) transmission rate
#' over 0 km.
#'
#' @param sdm_max_dist (class numeric) maximum distance (in km) over which short distance
#' mechanical (SDM) transmission can occur.
#'
#' @param sdm_scalar_lambda (class numeric) local scalar of short distance mechanical (SDM)
#' transmission.
#'
#' @return  (class list) of length 3 containing:
#' 1. (class matrix array) a matrix of site to site distances.
#' 2. (dgTMatrix, Matrix package) a matrix of distance-based transmission probabilities.
#' 3. (class data frame, sf package) input data frame of site catchment locality.
#'
#' @export
#'
#' @importFrom utils read.csv
#' @importFrom sf st_as_sf st_distance
#' @importFrom units drop_units
#' @importFrom igraph V get.vertex.attribute
#' @importFrom methods as
createDistanceMatrix <- function(graph,
                                 filename_site_catchments,
                                 crs_epsg,
                                 sdm_max_dist,
                                 sdm_rate_gamma,
                                 sdm_scalar_lambda) {
  # convert sdm_max_dist to m
  sdm_max_dist <- sdm_max_dist * 1000

  # import the list of site locations, and assign the correct spatial projection system
  site_catchments <- read.csv(filename_site_catchments, header = TRUE)
  site_catchments <- sf::st_as_sf(site_catchments,
                                  coords = c("easting", "northing"),
                                  crs = crs_epsg)

  # create a distance matrix (assign correct col and row names)
  matrix_distances <- sf::st_distance(site_catchments, site_catchments, by_element = F)
  matrix_distances <- units::drop_units(matrix_distances)
  dimnames(matrix_distances) <- list(site_catchments$siteID, site_catchments$siteID)

  # reorder distance matrix, so that it is in the same order as the contact matrix
  site_order <- igraph::get.vertex.attribute(graph = graph, name = "siteID", index = igraph::V(graph))
  matrix_distances_order <- matrix_distances[site_order, site_order]

  # exclude self-loops and ignore any distances longer than sdm_max_dist
  matrix_distances_order[cbind(site_order, site_order)] <- 0
  matrix_distances_order[matrix_distances_order > sdm_max_dist] <- 0

  # calculate probability of transmission, based on distance, if less than 0 or equal to sdm_rate_gamma reassign as 0
  matrix_distances_probability <- sdm_rate_gamma * exp(-(matrix_distances_order)^2 * sdm_scalar_lambda)
  matrix_distances_probability <- ifelse(matrix_distances_probability < 0, 0, matrix_distances_probability)
  matrix_distances_probability <- ifelse(matrix_distances_probability == sdm_rate_gamma, 0, matrix_distances_probability)
  matrix_distances_probability <- methods::as(matrix_distances_probability, "TsparseMatrix")

  # return list containing (1) site to site distances, (2) probability of transmission by distance,
  # and (3) data frame of site catchment information.
  return(list(matrix_distances_order = matrix_distances_order,
              matrix_distances_probability = matrix_distances_probability,
              site_catchments = site_catchments))
}
