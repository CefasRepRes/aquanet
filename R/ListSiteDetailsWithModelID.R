ListSiteDetailsWithModelID = function(graph_full, graph.contactp.objects, siteLocationsWithCatchment.fileName) {

  siteID <- igraph::V(graph_full)$siteID
  personID <- igraph::V(graph_full)$PersonID
  modelID <- 1:length(siteID)
  siteName <- igraph::V(graph_full)$siteName
  catchmentID <- igraph::V(graph_full)$catchmentID
  type <- igraph::V(graph_full)$type

  siteDetailsWithModelID <- data.frame(siteID, personID, modelID, siteName, catchmentID, type)

  site2Catchment.table <- read.csv(file = siteLocationsWithCatchment.fileName, header = TRUE)

  siteDetailsWithModelID.withCatchmentDetails <- merge(x = siteDetailsWithModelID,
                                                      y = site2Catchment.table,
                                                      by = c('siteID'),
                                                      all.x = TRUE)

  siteDetailsWithModelID.withCatchmentDetails <- siteDetailsWithModelID.withCatchmentDetails[order(siteDetailsWithModelID.withCatchmentDetails$modelID),]

  return(siteDetailsWithModelID.withCatchmentDetails)
}
