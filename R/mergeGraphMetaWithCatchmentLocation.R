#' mergeGraphMetaWithCatchmentLocation
#'
#' This function extracts metadata (siteID, PersonID, modelID, catchmentID, type) from the
#' connectivity graph and appends it to the site catchment locality information.
#'
#' @param graph (class igraph) graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-Mod. This includes both live fish movements and
#' Section 30 movements.
#'
#' @param filename_sites_catchments (class string) String containing the file path and file name for
#'  .csv containing information about site location (easting and northing) and which catchment each
#'  site resides in.
#'
#' @return (class data frame) Data frame containing metadata extracted from the sit connectivity
#' matrix produced with igraph and site location and catchment information.
#'
#' @export
#'
#' @importFrom utils read.csv
#' @importFrom igraph V
mergeGraphMetaWithCatchmentLocation <- function(graph,
                                                filename_sites_catchments) {

  # extract model and site metadata from igraph output to dataframe
  metadata <- data.frame(siteID = igraph::V(graph)$siteID,
                         modelID = 1:length(igraph::V(graph)$siteID),
                         catchmentID = igraph::V(graph)$CatchmentID)

  # read in data frame of site location and catchment information
  sites_catchments <- read.csv(file = filename_sites_catchments, header = TRUE)

  # merge model metadata and site catchment information
  metadata.sites_catchments <- merge(x = metadata,
                                     y = sites_catchments,
                                     by = "siteID",
                                     all.x = TRUE)

  # order data frame by modelID column
  metadata.sites_catchments <- metadata.sites_catchments[order(metadata.sites_catchments$modelID), ]

  # return merged data frame
  return(metadata.sites_catchments)
}
