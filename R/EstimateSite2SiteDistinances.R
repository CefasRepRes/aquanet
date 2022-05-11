CreateDistanceMatrix <- function(graph, filename_site_catchments) {
  # Define the British National Grid Referencing System, using Proj4 notation
  britishNationalGrid <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'

  # Import the list of site locations, and assign the correct spatial projection system
  site_catchments <- read.csv(filename_site_catchments, header = TRUE)
  sp::coordinates(site_catchments) <- c('easting', 'northing')
  sp::proj4string(site_catchments) <- sp::CRS(britishNationalGrid)

  # Create a distance matrix
  site_catchments_distances <- sp::spDists(site_catchments)
  dimnames(site_catchments_distances) <- list(site_catchments@data$siteID, site_catchments@data$siteID)

  # Reorder matrix, so that it is in the same order as the contact matrix
  site_order <- igraph::get.vertex.attribute(graph = graph, name = "siteID", index = igraph::V(graph))
  site_catchment_distances_order <- site_catchments_distances[site_order, site_order]

  # Exclude self-loops and ignore any distances longer than 5000m
  site_catchment_distances_order[cbind(site_order,site_order)] <- 0
  site_catchment_distances_order[site_catchment_distances_order > 5000] <- 0

  # Calculate probability of transmission, based on distance
  ListSiteLocations.withCatchment.probability.reordered <- 0.005 * exp(-(site_catchment_distances_order)^2 * 0.000001)
  ListSiteLocations.withCatchment.probability.reordered <- ifelse(ListSiteLocations.withCatchment.probability.reordered < 0, 0, ListSiteLocations.withCatchment.probability.reordered)
  ListSiteLocations.withCatchment.probability.reordered <- ifelse(ListSiteLocations.withCatchment.probability.reordered == 0.005, 0, ListSiteLocations.withCatchment.probability.reordered)

  ListSiteLocations.withCatchment.probability.reordered <- methods::as(ListSiteLocations.withCatchment.probability.reordered, "dgTMatrix")

  return(list(site_catchment_distances_order, ListSiteLocations.withCatchment.probability.reordered, site_catchments))
}
