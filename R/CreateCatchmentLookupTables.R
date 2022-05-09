CreateCatchment2SiteMatrix <- function(graph, filename_catchment_layer) {
  # Create a table 'graph.catchment2site.Merged' which maps site to catchment
  # Ensure that the order of sites matches that included within the contact matrix

  graph.catchmentID <- igraph::get.vertex.attribute(graph = graph,
                                                    name = "catchmentID",
                                                    index = igraph::V(graph))

  graph.siteID <- igraph::get.vertex.attribute(graph = graph,
                                               name = "siteID",
                                               index = igraph::V(graph))

  graph.catchment2site <- as.data.frame(cbind(graph.catchmentID, graph.siteID))

  colnames(graph.catchment2site) <- c("TRUNK_CODE", "siteID")

  graph.catchment2site$Order <- seq(1, nrow(graph.catchment2site))

  catchmentLayer <- rgdal::readOGR(dsn = filename_catchment_layer,
                                   layer = "catchmnt_50k+TrunkCodes-Filtered-Merged_region")

  catchmentLayer.Table <- catchmentLayer@data

  graph.catchment2site.merged <- merge(x = graph.catchment2site,
                                       y = catchmentLayer.Table,
                                       by.x = 'TRUNK_CODE',
                                       by.y = 'TRUNK_CODE',
                                       sort = FALSE,
                                       all.x = TRUE)

  graph.catchment2site.merged <- graph.catchment2site.merged[order(graph.catchment2site.merged$Order), ]

  # Transform the catchment2site table into a logical matrix
  # Keep sites in the same order
  graph.catchment2site$TRUNK_CODE <- factor(x = graph.catchment2site$TRUNK_CODE,
                                            levels = unique(graph.catchment2site$TRUNK_CODE),
                                            ordered = TRUE)

  row.names(graph.catchment2site) <- graph.catchment2site$siteID

  graph.catchment2site.matrix <- model.matrix(~ 0 + TRUNK_CODE,data = graph.catchment2site)

  graph.catchment2site.matrix2 <- Matrix::Matrix(graph.catchment2site.matrix > 0, sparse = TRUE)

  graph.catchment2site.matrix2 <- as(object = graph.catchment2site.matrix2, Class = "dgCMatrix")

  return(list(graph.catchment2site.merged, graph.catchment2site.matrix2))
}
