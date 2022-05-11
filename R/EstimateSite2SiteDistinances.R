CreateDistanceMatrix <- function(graph_full, siteLocationsWithCatchment.fileName, ListModelSetupParameters) {
  # Define the British National Grid Referencing System, using Proj4 notation
  britishNationalGrid <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
  siteLocationsWithCatchment.fileName <- siteLocationsWithCatchmentDuplicatesRemoved.fileName
  # Import the list of site locations, and assign the correct spatial projection system
  ListSiteLocations.withCatchment <- read.csv(siteLocationsWithCatchment.fileName, header = TRUE)
  coordinates(ListSiteLocations.withCatchment) <- c('easting','northing')
  proj4string(ListSiteLocations.withCatchment) <- sp::CRS(britishNationalGrid)

  # Create a distance matrix
  ListSiteLocations.withCatchment.distance <- sp::spDists(ListSiteLocations.withCatchment)
  dimnames(ListSiteLocations.withCatchment.distance) <- list(ListSiteLocations.withCatchment@data$siteID, ListSiteLocations.withCatchment@data$siteID)

  # Reorder matrix, so that it is in the same order as the contact matrix
  graph.siteID.order <- igraph::get.vertex.attribute(graph = graph_full, name = "siteID",index = V(graph_full))
  ListSiteLocations.withCatchment.distance.reordered <- ListSiteLocations.withCatchment.distance[graph.siteID.order, graph.siteID.order]

  # Exclude self-loops and ignore any distances longer than 5000m
  ListSiteLocations.withCatchment.distance.reordered[cbind(graph.siteID.order,graph.siteID.order)] <- 0
  ListSiteLocations.withCatchment.distance.reordered[ListSiteLocations.withCatchment.distance.reordered > 5000] <- 0 ####

  # Calculate probability of transmission, based on distance
  ListSiteLocations.withCatchment.probability.reordered <- 0.005*exp(-(ListSiteLocations.withCatchment.distance.reordered)^2*0.000001)
  ListSiteLocations.withCatchment.probability.reordered <- ifelse(ListSiteLocations.withCatchment.probability.reordered < 0, 0, ListSiteLocations.withCatchment.probability.reordered)
  ListSiteLocations.withCatchment.probability.reordered <- ifelse(ListSiteLocations.withCatchment.probability.reordered == 0.005, 0, ListSiteLocations.withCatchment.probability.reordered)

  ListSiteLocations.withCatchment.probability.reordered <- methods::as(ListSiteLocations.withCatchment.probability.reordered, "dgTMatrix")

  return(list(ListSiteLocations.withCatchment.distance.reordered, ListSiteLocations.withCatchment.probability.reordered, ListSiteLocations.withCatchment))
}
