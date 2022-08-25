#' checkCatchmentLevelRestocking
#'
#' This function identifies catchments that are ready to restock as all sites within them are in a
#' post-fallow state (see details).
#'
#' TODO: control matrix not altered here - remove output?
#' TODO: is catchments_with_fallow_some output used?
#'
#' Function takes control matrix stating which sites are in a fallow state and which are in a post-
#' fallow state (ready for restocking). This information is combined with the siteID-to-catchment
#' matrix to calculate the number of sites in a fallow or post-fallow state within each catchment.
#' This information is then used to determine which catchments contain only sites in a post-fallow
#' state that are ready for restocking and to determine which catchments contain sites in a fallow
#' state.
#'
#' @param control_matrix (class matrix) matrix containing 7 columns depicting different control
#' states and rows (of length number of sites) depicting whether each sites is 1 = in the specified
#' control state or 0 = not in the specified control state.
#'
#' @param spmatrix_sites_catchment (class dgCMatrix, Matrix package) sparse matrix containing site
#' to catchment summary.
#' Note: this is created with the `aquanet::createCatchmentToSiteMatrix()` function.
#'
#' @param n_catchments (class integer) number of catchments.
#'
#' @return (class list) of length 3 containing:
#' 1. (class matrix) input `control_matrix` containing 7 columns depicting different control
#' states and rows (of length number of sites) depicting whether each sites is 1 = in the specified
#' control state or 0 = not in the specified control state. This matrix is unedited and output to
#' keep a record of the input data used to generate these catchment restocking data.
#' 2. (class logical) vector of length number of catchments (`n_catchments`) depicting whether
#' catchments contain fallow sites.
#' 3. (class logical) vector of length number of catchments (`n_catchments`) depicting whether
#' catchments contain only post-fallow sites ready for restocking with no fallow sites.
#'
#' @export
#'
#' @importFrom Matrix t
checkCatchmentLevelRestocking <- function(control_matrix,
                                          spmatrix_sites_catchment,
                                          n_catchments) {
  # create binary vector of sites in fallow or post-fallow (awaiting restock) state
  # TODO: columns names instead of column numbers
  sites_fallow <- control_matrix[ , 4]
  sites_post_fallow <- control_matrix[ , 5]

  # create vector with number of sites in fallow or post-fallow state in each catchment
  catchment_n_sites_fallow <- as.vector(Matrix::t(spmatrix_sites_catchment) %*% sites_fallow)
  catchment_n_sites_post_fallow <- as.vector(Matrix::t(spmatrix_sites_catchment) %*% sites_post_fallow)

  # create logical vector of catchments with more than one fallow or post-fallow site
  catchment_with_fallow <- catchment_n_sites_fallow > 0
  catchment_with_post_fallow <- catchment_n_sites_post_fallow > 0

  # create logical vector of catchments with only post-fallow sites and no fallow sites
  catchments_with_post_fallow_only <- rep(FALSE, n_catchments)
  catchments_with_post_fallow_only[catchment_with_post_fallow] <- catchment_n_sites_fallow[catchment_with_post_fallow] == 0

  # create logical vector of catchments with fallow sites
  catchments_with_fallow_some <- rep(FALSE, n_catchments)
  catchments_with_fallow_some[catchment_with_fallow] <- catchment_n_sites_fallow[catchment_with_fallow] != 0

  # TODO name list elements
  return(list(control_matrix, catchments_with_fallow_some, catchments_with_post_fallow_only))

}
