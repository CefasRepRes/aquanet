#' createRiverDistanceProbabilityMatrix
#'
#' This function creates a spase matrix for river distance-related transmission probabilities
#' between sites for disease transmission via RB route of AquaNet-Mod (see details).
#'
#' Generate a sparse matrix of site to site transmission probabilities via river distances between
#' sites connected by rivers (due to close proximity to the river network). Distances of zero are removed. Distances are converted to transmission
#' probabilities using the equation
#' `p1km * ((max_dist - (river_distance_length/1000))/(max_dist - 1))` and probabilities less than
#' zero are corrected. A sparse matrix with site IDs in the same order as the contact matrix is
#' generated and populated with the river distance transmission probabilities. The resulting list
#' contains the river distance data frame used and a river distance transmission probability
#' sparse matrix.
#'
#' @param river_distances (class data frame) data frame of river distances created by `loadRiverDistances` function of aquanet.
#'
#' @param out_createContactProbabilityMatrix (class list) of length 3 containing:
#' 1. (class integer) number of sites in movements matrix (integer).
#' 2. (class dgCMatrix, Matrix package) movements matrix.
#' 3. (class dgCMatrix, Matrix package) probability of movements matrix with top sites zeroed.
#'
#' @param max_dist (class numeric) number stating maximum distance (km) that a pathogen can travel
#' in water via river network.
#'
#' @param p1km (class numeric) probability that a site 1 km downstream of infection becomes infected.
#'
#' @return (class list) of length 2 containing:
#' 1. (class data frame) distances between sites on a river network (via river connectivity)
#' connections of 0 distances are removed.
#' 2. (class dgTMatrix, Matrix package) sparse matrix containing probability of transmission
#' between sites connected via the river network by river water.
#'
#' @export
#'
#' @importFrom utils read.csv
#' @importFrom Matrix Matrix
#'
createRiverDistanceProbabilityMatrix <- function(river_distances,
                                                 out_createContactProbabilityMatrix,
                                                 max_dist,
                                                 p1km) {

  # rename river distances
  river_distances_rm0 <- river_distances

  # create vector of sites in the same order as the adjacency matrix
  vector_sites <- as.numeric(out_createContactProbabilityMatrix[["matrix_movements_prob"]]@Dimnames[[1]])

  # extract number of sites from the contact probability matrix
  n_sites <- out_createContactProbabilityMatrix[["n_sites"]]

  # convert site to site river distances to probabilities correcting for probabilities below zero
  river_distances_rm0$calcProb <- (p1km *
                                     ((max_dist -
                                         (river_distances_rm0$Total_Length / 1000))/(max_dist - 1)))

  river_distances_rm0$calcProb <- ifelse(river_distances_rm0$calcProb < 0,
                                         yes = 0,
                                         no = river_distances_rm0$calcProb)

  # create edge list of origin and destination river sites
  river_distances_rm0_edges <- river_distances_rm0[, c("Origin.SiteID", "Dest.SiteID")]

  # express each siteID as a factor
  # levels assigned based on the site's position within the original contact matrix
  river_distances_rm0_edges$Origin.SiteID <- factor(x = river_distances_rm0_edges$Origin.SiteID,
                                                    levels = vector_sites)

  river_distances_rm0_edges$Dest.SiteID <- factor(x = river_distances_rm0_edges$Dest.SiteID,
                                                  levels = vector_sites)

  # create edge list, each site is expressed in terms of position within the original adjacency matrix
  river_distances_rm0_edges$Origin.Matrix.Pos <- as.numeric(river_distances_rm0_edges$Origin.SiteID)
  river_distances_rm0_edges$Dest.Matrix.Pos <- as.numeric(river_distances_rm0_edges$Dest.SiteID)

  # create empty matrix to store river distances between sites
  matrix_river_distances_prob <- Matrix::Matrix(data = 0,
                                         nrow = n_sites,
                                         ncol = n_sites,
                                         dimnames = list(as.character(vector_sites),
                                                         as.character(vector_sites)))

  # insert site to site transmission probabilities via river to the matrix
  matrix_river_distances_prob[cbind(river_distances_rm0_edges$Origin.Matrix.Pos,
                             river_distances_rm0_edges$Dest.Matrix.Pos)] <- river_distances_rm0$calcProb

  # return list containing (1) river distances and (2) river transmission probability matrix
  return(list(river_distances_rm0 = river_distances_rm0,
              matrix_river_distances_prob = matrix_river_distances_prob))
}
