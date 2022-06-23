checkCatchmentLevelRestocking <- function(control_matrix,
                                          tdiff,
                                          graph.catchment2site.matrix2,
                                          no.catchments) {
  # create binary vector of sites in fallow or post-fallow (awaiting restock) state
  # TODO: columns names instead of column numbers
  sites_fallow <- control_matrix[ , 4]
  sites_post_fallow <- control_matrix[ , 5]

  # create vector with number of sites in fallow or post-fallow state in each catchment
  catchment_n_sites_fallow <- as.vector(t(graph.catchment2site.matrix2) %*% sites_fallow)
  catchment_n_sites_post_fallow <- as.vector(t(graph.catchment2site.matrix2) %*% sites_post_fallow)

  # filter catchments with more than one fallow or post_fallow site
  catchment_with_fallow <- catchment_n_sites_fallow > 0
  catchment_with_post_fallow <- catchment_n_sites_post_fallow > 0

  # Identify catchments that contain sites ready to be restocked, but no fallow sites
  catchments.all.sites.c5.status <- rep(FALSE, no.catchments)
  catchments.all.sites.c5.status[catchment_with_post_fallow] <- catchment_n_sites_fallow[catchment_with_post_fallow] == 0

  # Identify catchments that contain fallow sites
  catchments.some.sites.c4.status <- rep(FALSE, no.catchments)
  catchments.some.sites.c4.status[catchment_with_fallow] <- catchment_n_sites_fallow[catchment_with_fallow] != 0

  return(list(control_matrix, catchments.some.sites.c4.status, catchments.all.sites.c5.status))

}
