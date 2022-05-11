CreateDistanceMatrix <- function(graph, filename_site_catchments) {
  # define the British National Grid Referencing System, using Proj4 notation
  britishNationalGrid <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'

  # import the list of site locations, and assign the correct spatial projection system
  site_catchments <- read.csv(filename_site_catchments, header = TRUE)
  sp::coordinates(site_catchments) <- c('easting', 'northing')
  sp::proj4string(site_catchments) <- sp::CRS(britishNationalGrid)

  # create a distance matrix (assign correct col and row names)
  site_catchments_distances <- sp::spDists(site_catchments)
  dimnames(site_catchments_distances) <- list(site_catchments@data$siteID, site_catchments@data$siteID)

  # reorder distance matrix, so that it is in the same order as the contact matrix
  site_order <- igraph::get.vertex.attribute(graph = graph, name = "siteID", index = igraph::V(graph))
  site_catchment_distances_order <- site_catchments_distances[site_order, site_order]

  # exclude self-loops and ignore any distances longer than 5000 m
  site_catchment_distances_order[cbind(site_order, site_order)] <- 0
  site_catchment_distances_order[site_catchment_distances_order > 5000] <- 0

  # calculate probability of transmission, based on distance, if less than 0 or equal to 0.005 reassign as 0
  matrix_distances_probability <- 0.005 * exp(-(site_catchment_distances_order)^2 * 0.000001)
  matrix_distances_probability <- ifelse(matrix_distances_probability < 0, 0, matrix_distances_probability)
  matrix_distances_probability <- ifelse(matrix_distances_probability == 0.005, 0, matrix_distances_probability)
  matrix_distances_probability <- methods::as(matrix_distances_probability, "dgTMatrix")

  # return list containing (1) site to site distances, (2) probability of transmission by distance,
  # and (3) data frame of site catchment information.
  return(list(site_catchment_distances_order, matrix_distances_probability, site_catchments))
}
