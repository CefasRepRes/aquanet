ListSiteDetailsWithModelID = function(graph, siteLocationsWithCatchment.fileName) {

  siteID <- igraph::V(graph)$siteID
  personID <- igraph::V(graph)$PersonID
  modelID <- 1:length(siteID)
  siteName <- igraph::V(graph)$siteName
  catchmentID <- igraph::V(graph)$catchmentID
  type <- igraph::V(graph)$type

  siteDetailsWithModelID <- data.frame(siteID, personID, modelID, siteName, catchmentID, type)

  site2Catchment.table <- read.csv(file = siteLocationsWithCatchment.fileName, header = TRUE)

  siteDetailsWithModelID.withCatchmentDetails <- merge(x = siteDetailsWithModelID,
                                                      y = site2Catchment.table,
                                                      by = c('siteID'),
                                                      all.x = TRUE)

  siteDetailsWithModelID.withCatchmentDetails <- siteDetailsWithModelID.withCatchmentDetails[order(siteDetailsWithModelID.withCatchmentDetails$modelID),]

  return(siteDetailsWithModelID.withCatchmentDetails)
}
