createCatchmentToSiteMatrix <- function(graph, filename_catchment_layer) {
  # Create a table 'df_sites.Merged' which maps site to catchment
  # Ensure that the order of sites matches that included within the contact matrix

  # create data frame of catchment ID and site ID
  df_sites <- data.frame("TRUNK_CODE" = igraph::get.vertex.attribute(graph = graph,
                                                                     name = "catchmentID",
                                                                     index = igraph::V(graph)),
                         "siteID" = igraph::get.vertex.attribute(graph = graph,
                                                                 name = "siteID",
                                                                 index = igraph::V(graph)),
                         "Order" = seq(1, length(get.vertex.attribute(graph = graph, "siteID"))))

  # load GIS catchment layer shapefile to SpatialPolygonsDataFrame
  catchment_layer <- rgdal::readOGR(dsn = filename_catchment_layer,
                                   layer = sub(pattern = "(.*)\\..*$",
                                               replacement = "\\1",
                                               basename(filename_catchment_layer)))

  # extract the data from the catchment layer
  df_catchments <- catchment_layer@data

  # merge catchment data with site data extracted from graph
  df_catchment_sites <- merge(x = df_sites,
                              y = df_catchments,
                              by.x = "TRUNK_CODE",
                              by.y = "TRUNK_CODE",
                              sort = FALSE,
                              all.x = TRUE)

  # order the merged data frame
  df_catchment_sites <- df_catchment_sites[order(df_catchment_sites$Order), ]

  # convert catchment codes to ordered factor to retain site order
  df_sites$TRUNK_CODE <- factor(x = df_sites$TRUNK_CODE,
                                            levels = unique(df_sites$TRUNK_CODE),
                                            ordered = TRUE)

  # assign siteID as rownames
  row.names(df_sites) <- df_sites$siteID

  # convert to sparse matrix (rows are siteID, cols are catchment code)
  matrix_sites_catchment <- stats::model.matrix(~ 0 + TRUNK_CODE, data = df_sites)
  spmatrix_sites_catchment <- Matrix::Matrix(matrix_sites_catchment > 0, sparse = TRUE)
  spmatrix_sites_catchment <- methods::as(object = spmatrix_sites_catchment, Class = "dgCMatrix")

  # return a list containing (1) data frame of catchment and (2) site data and site to catchment matrix
  return(list(df_catchment_sites, spmatrix_sites_catchment))
}
