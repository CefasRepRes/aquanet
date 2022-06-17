checkCatchmentLevelRestocking <- function(control_matrix,
                                          tdiff,
                                          graph.catchment2site.matrix2,
                                          no.catchments) {
  # Extract a list of sites that are fallow, and those which are waiting to be restocked
  controlled.sites.c4.numeric <- control_matrix[ , 4]
  controlled.sites.c5.numeric <- control_matrix[ , 5]

  # List the number of sites that are fallow, and are waiting to be restocked, in each catchment
  catchments.no.c4.sites.present <- as.vector(t(graph.catchment2site.matrix2) %*% controlled.sites.c4.numeric)
  catchments.no.c5.sites.present <- as.vector(t(graph.catchment2site.matrix2) %*% controlled.sites.c5.numeric)

  # List catchments containing fallow sites, and those which are waiting to be restocked
  catchments.c4.sites.present.logical <- catchments.no.c4.sites.present > 0
  catchments.c5.sites.present.logical <- catchments.no.c5.sites.present > 0

  # Identify catchments that contain sites ready to be restocked, but no fallow sites
  catchments.all.sites.c5.status <- rep(FALSE, no.catchments)
  catchments.all.sites.c5.status[catchments.c5.sites.present.logical] <- catchments.no.c4.sites.present[catchments.c5.sites.present.logical] == 0

  # Identify catchments that contain fallow sites
  catchments.some.sites.c4.status <- rep(FALSE, no.catchments)
  catchments.some.sites.c4.status[catchments.c4.sites.present.logical] <- catchments.no.c4.sites.present[catchments.c4.sites.present.logical] != 0

  return(list(control_matrix, catchments.some.sites.c4.status, catchments.all.sites.c5.status))

}
