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
