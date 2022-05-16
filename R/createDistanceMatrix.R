#' createDistanceMatrix
#'
#' Using the connectivity matrix and site to catchment information .csv create a matrix of distances
#'  between these sites calculated using the sp package and British National Grid Referencing
#'  system. Additionally, filter these distances to remove self loops and distances greater than
#'  5000 m (assume greater than 5000 m constitutes negligible transmission risk). Convert distances
#'  to a probability of transmission with the following equation
#'  `0.005 * exp(-(distance)^2 * 0.000001)`. Convert probabilities that are less than 0 or equal to
#'  0.005 to 0. Return a list containing site-site distance matrix, site-site distance-based
#'  transmission probability matrix and site-catchment data frame. Note: matrices produces have the
#'  same reorder distance matrix, so that it is in the same site order as the contact matrix
#'  (graph).
#'
#' @param graph  (class igraph) Graph of connections/movements between sites produced with iGraph
#' (using script importSiteData.R of AquaNet-Mod). This includes both live fish movements and
#' Section 30 movements.
#' @param filename_site_catchments  (class string) String containing the file path and file name for
#'  .csv containing information about site location (easting and northing) and which catchment each
#'  site resides in
#' @param crs_epsg (class numeric) 4-5 digit epsg code stating the coordinate reference system (crs)
#'  to use for projecting the data.
#'
#' @return  (class list) of length 3 containing (1) a matrix of site to site distances (class matrix
#'  array), (2) a matrix of distance-based transmission probabilities (dgTMatrix, Matrix package),
#'  and (3) input data frame of site catchment locality (SpatialPointsDataFrame, sp package).
#'
#' @export
#'
#' @importFrom utils read.csv
#' @importFrom sf st_as_sf st_distance
#' @importFrom units drop_units
#' @importFrom igraph V get.vertex.attribute
#' @importFrom methods as
createDistanceMatrix <- function(graph, filename_site_catchments, crs_epsg) {
  # define the British National Grid Referencing System, using Proj4 notation

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

  # exclude self-loops and ignore any distances longer than 5000 m
  matrix_distances_order[cbind(site_order, site_order)] <- 0
  matrix_distances_order[matrix_distances_order > 5000] <- 0

  # calculate probability of transmission, based on distance, if less than 0 or equal to 0.005 reassign as 0
  matrix_distances_probability <- 0.005 * exp(-(matrix_distances_order)^2 * 0.000001)
  matrix_distances_probability <- ifelse(matrix_distances_probability < 0, 0, matrix_distances_probability)
  matrix_distances_probability <- ifelse(matrix_distances_probability == 0.005, 0, matrix_distances_probability)
  matrix_distances_probability <- methods::as(matrix_distances_probability, "dgTMatrix")

  # return list containing (1) site to site distances, (2) probability of transmission by distance,
  # and (3) data frame of site catchment information.
  return(list(matrix_distances_order, matrix_distances_probability, site_catchments))
}
