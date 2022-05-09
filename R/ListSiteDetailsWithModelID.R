mergeGraphMetaWithCatchmentLocation <- function(graph, filename_sites_catchments) {

  # extract model and site metadata from igraph output to dataframe
  model_metadata <- data.frame(siteID = igraph::V(graph)$siteID,
                         personID = igraph::V(graph)$PersonID,
                         modelID = 1:length(igraph::V(graph_full)$siteID),
                         siteName = igraph::V(graph)$siteName,
                         catchmentID = igraph::V(graph)$catchmentID,
                         type = igraph::V(graph)$type)

  # read in data frame of site location and catchment information
  sites_catchments <- read.csv(file = filename_sites_catchments, header = TRUE)

  # merge model metadata and site catchment information
  model_metadata.sites_catchments <- merge(x = model_metadata,
                                           y = sites_catchments,
                                           by = "siteID",
                                           all.x = TRUE)

  # order data frame by modelID column
  model_metadata.sites_catchments <- model_metadata.sites_catchments[order(model_metadata.sites_catchments$modelID), ]

  # return merged data frame
  return(model_metadata.sites_catchments)
}
