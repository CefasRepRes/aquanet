checkCatchmentLevelRestocking <- function(control_matrix,
                                          tdiff,
                                          graph.catchment2site.matrix2,
                                          no.catchments) {
  # create binary vector of sites in fallow or post-fallow (awaiting restock) state
  sites_fallow <- control_matrix[ , 4]
  sites_post_fallow <- control_matrix[ , 5]

  # List the number of sites that are fallow, and are waiting to be restocked, in each catchment
  catchment_n_sites_fallow <- as.vector(t(graph.catchment2site.matrix2) %*% sites_fallow)
  catchment_n_sites_post_fallow <- as.vector(t(graph.catchment2site.matrix2) %*% sites_post_fallow)

  # List catchments containing fallow sites, and those which are waiting to be restocked
  catchments.c4.sites.present.logical <- catchment_n_sites_fallow > 0
  catchments.c5.sites.present.logical <- catchment_n_sites_post_fallow > 0

  # Identify catchments that contain sites ready to be restocked, but no fallow sites
  catchments.all.sites.c5.status <- rep(FALSE, no.catchments)
  catchments.all.sites.c5.status[catchments.c5.sites.present.logical] <- catchment_n_sites_fallow[catchments.c5.sites.present.logical] == 0

  # Identify catchments that contain fallow sites
  catchments.some.sites.c4.status <- rep(FALSE, no.catchments)
  catchments.some.sites.c4.status[catchments.c4.sites.present.logical] <- catchment_n_sites_fallow[catchments.c4.sites.present.logical] != 0

  return(list(control_matrix, catchments.some.sites.c4.status, catchments.all.sites.c5.status))

}
