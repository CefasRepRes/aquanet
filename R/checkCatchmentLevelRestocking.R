checkCatchmentLevelRestocking <- function(control_matrix,
                                          graph.catchment2site.matrix2,
                                          n_catchments) {
  # create binary vector of sites in fallow or post-fallow (awaiting restock) state
  # TODO: columns names instead of column numbers
  sites_fallow <- control_matrix[ , 4]
  sites_post_fallow <- control_matrix[ , 5]

  # create vector with number of sites in fallow or post-fallow state in each catchment
  catchment_n_sites_fallow <- as.vector(Matrix::t(graph.catchment2site.matrix2) %*% sites_fallow)
  catchment_n_sites_post_fallow <- as.vector(Matrix::t(graph.catchment2site.matrix2) %*% sites_post_fallow)

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
