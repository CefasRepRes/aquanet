#' createCatchmentToSiteMatrix
#'
#' This function produces a sparse matrix detailing which catchments each site is situated in (see
#' details).
#'
#' Extract connectivity matrix (graph) information to produce a data frame of siteID linked to
#' catchment code (TRUNK_CODE) and merge with detailed catchment information from GIS layer data
#' (.shp). This produces list output 1 which which maps site to catchment. To produce list output 2,
#' convert the catchment (columns) to site (rows) information to a sparse matrix. Note: ensure that
#'  the order of sites matches that included within the contact matrix.
#'
#' @param graph (class igraph) Graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-mod. This includes both live fish movements and
#' Section 30 movements.
#'
#' @param filename_catchment_layer (class string) String containing the file path and file name for
#' .shp file containing catchment information.
#'
#' @param crs_epsg (class numeric) 4-5 digit epsg code stating the coordinate reference system (crs)
#'  to use for projecting the data.
#'
#' @return (class list) of length 2 containing:
#' 1. (class data frame) data frame of site to catchment information.
#' 2. (class dgCMatrix) sparse matrix containing site to catchment summary.
#'
#' @export
#'
#' @importFrom igraph get.vertex.attribute V
#' @importFrom sf read_sf st_transform
#' @importFrom stats model.matrix
#' @importFrom Matrix Matrix
#' @importFrom methods as
createCatchmentToSiteMatrix <- function(graph, filename_catchment_layer, crs_epsg) {


  # create data frame of catchment ID and site ID
  df_sites <- data.frame(TRUNK_CODE = igraph::get.vertex.attribute(graph = graph,
                                                                   name = "CatchmentID",
                                                                   index = igraph::V(graph)),
                         siteID = igraph::get.vertex.attribute(graph = graph,
                                                               name = "siteID",
                                                               index = igraph::V(graph)),
                         Order = seq(1, length(get.vertex.attribute(graph = graph, "siteID"))))

  #Edit TRUNK_code to have 00 at the start to match catchement format)
  df_sites$TRUNK_CODE <- sprintf("00%s", df_sites$TRUNK_CODE)

  # load GIS catchment layer shapefile to SpatialPolygonsDataFrame
  catchment_layer_crs <- sf::read_sf(dsn = filename_catchment_layer,
                                     layer = sub(pattern = "(.*)\\..*$",
                                                 replacement = "\\1",
                                                 basename(filename_catchment_layer)))

  # transform to correct crs epsg code (if already correct, does nothing)
  #catchment_layer_crs <- sf::st_transform(catchment_layer, crs = crs_epsg) # commented out as create warnings if in the correct crs

  # extract the data from the catchment layer
  df_catchments <- as.data.frame(catchment_layer_crs)
  cols <- c("ESW_CatID", "ESW_CNAM")
  df_catchments[cols] <- sapply(df_catchments[cols], as.character)

  # merge catchment data with site data extracted from graph
  df_catchment_sites <- merge(x = df_sites,
                              y = df_catchments,
                              by.x = "TRUNK_CODE",
                              by.y = "ESW_CatID",
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
  spmatrix_sites_catchment <- methods::as(object = spmatrix_sites_catchment, Class = "CsparseMatrix")

  # return a list containing (1) data frame of catchment and (2) site data and site to catchment matrix
  return(list(df_catchment_sites = df_catchment_sites,
              spmatrix_catchment_sites = spmatrix_sites_catchment))
}

#' createWithinCatchmentEdgesMatrix
#'
#' This function produces a logical matrix detailing within catchment connections (see details).
#'
#' Extract connectivity matrix (graph) information at "withinCatchment" level to produce a logical
#' matrix of within catchment connections, a matrix of within catchment edges and a matrix of within
#'  catchment edges by source (column 1) and receiving (column 2) site ID.
#'
#' @param graph (class igraph) Graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-mod. This includes both live fish movements and
#' Section 30 movements.
#'
#' @return (class list) of length 3 containing:
#' 1. (class lgCMatrix) logical matrix detailing within catchment connections.
#' 2. (class matrix) edge matrix of vertex IDs within catchments.
#' 3. (class matrix) matrix of source site and receiving site within catchment edges.
#'
#' @export
#'
#' @importFrom igraph get.adjacency get.edges E
createWithinCatchmentEdgesMatrix <- function(graph) {
  # create logical matrix to specify which edges represent movements within a catchment
  lgmatrix_catch_catch <- igraph::get.adjacency(graph = graph,
                                                attr = "withinCatchment",
                                                names = TRUE,
                                                sparse = TRUE)

  # get the position of each within catchment movement as edge table
  matrix_edges_within_catch <- igraph::get.edges(graph = graph,
                                                 igraph::E(graph)[igraph::E(graph)$withinCatchment])

  # create edge list of within catchment movements by source and receiving site ID
  matrix_edges_within_catch_siteID <- cbind(igraph::E(graph)$scrSiteID[igraph::E(graph)$withinCatchment],
                                            igraph::E(graph)$recSiteID[igraph::E(graph)$withinCatchment])
  colnames(matrix_edges_within_catch_siteID) <- c('scrSiteID','recSiteID')

  # return list containing logical matrix of catchment:catchment connections,
  # numeric matrix of within catchment edges, and siteID-siteID within catchment edges
  return(list(lgmatrix_catch_catch = lgmatrix_catch_catch,
              matrix_edges_within_catch = matrix_edges_within_catch,
              matrix_edges_within_catch_siteID = matrix_edges_within_catch_siteID))
}
