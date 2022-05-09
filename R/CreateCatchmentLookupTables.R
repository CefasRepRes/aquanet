#' createCatchmentToSiteMatrix
#'
#' Extract connectivity matrix (graph) information to produce a data frame of siteID linked to catchment code (TRUNK_CODE) and merge with detailed catchment information from GIS layer data (.shp). This produced list output 1 which which maps site to catchment. To produce list output 2, convert the catchment (columns) to site (rows) information to a sparse matrix. Note: ensure that the order of sites matches that included within the contact matrix.
#'
#' @param graph (class igraph) Graph of connections/movements between sites produced with iGraph (using script importSiteData.R of AquaNet-Mod). This includes both live fish movements and Section 30 movements.
#' @param filename_catchment_layer (class string) String containing the file path and file name for .shp file containing catchment information.
#'
#' @return (class list) of length 2 containing (1) data frame of site to catchment information and (2) dgCMatrix sparse matrix containing site to catchment summary.
#'
#' @importFrom igraph get.vertex.attribute V
#' @importFrom rgdal readOGR
#' @importFrom stats model.matrix
#' @importFrom Matrix Matrix
#' @importFrom methods as
createCatchmentToSiteMatrix <- function(graph, filename_catchment_layer) {
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
