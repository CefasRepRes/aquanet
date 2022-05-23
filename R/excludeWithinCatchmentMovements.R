excludeWithinCatchmentMovements <- function(move_restricted_sites, spmatrix_risk_contacts, catchment_movements) {
  # TODO: contactp input not define
  # TODO: replace list numbers with named elements
  # extract elements from list
  spmatrix_sites_catchment <- catchment_movements[[1]]
  lgmatrix_catch_catch <- catchment_movements[[2]]
  catchments_controlled_prev <- catchment_movements[[3]]
  matrix_contacts_exclude <- catchment_movements[[4]]
  site_control_type <- catchment_movements[[5]]

  # create matrix of catchments (rows) under control (col 1) by multiplying the sites by whether movements are restricted
  catchments_controlled <- t(spmatrix_sites_catchment) %*% move_restricted_sites

  # determine number of catchments under control
  n_catchments_controlled <- sum(catchments_controlled > 0)

  # if the catchments under control are NOT the same as last time step:
  if (!all(catchments_controlled@x == catchments_controlled_prev@x)) {
    # return vector of sites contained within the controlled catchments and correct those over 1
    sites_controlled <- as.vector(spmatrix_sites_catchment %*% catchments_controlled)
    sites_controlled[sites_controlled > 1] <- 1

    # create contact probability matrix for sites within controlled catchments
    # sites outside controlled catchments have p = 0 else 1
    contacts.by.controlledSites <- contactp * sites_controlled
    contacts.by.controlledSites[contacts.by.controlledSites > 0] <- 1

    # reassign the secondary controlled sites element with sites under catchment controls in this time step
    catchment_movements[[6]] <- sites_controlled

    # if the site control type is 1 or 2
    if (site_control_type %in% c(0,1)) {
      # create matrix of all contacts made within controlled catchments
      contacts.withinCatchment.by.controlledSites <- contacts.by.controlledSites * lgmatrix_catch_catch

      # create matrix of all contacts made outside of controlled catchments
      matrix_contacts_exclude <- contacts.by.controlledSites - contacts.withinCatchment.by.controlledSites
    }

    # if the site control type is 1 (allow movements within or between infected catchments)
    if (site_control_type == 1) {
      # create matrix of contacts made to other sites within controlled catchments
      contacts.between.controlled.catchments <- t(contacts.by.controlledSites) * sites_controlled
      contacts.between.controlled.catchments <- t(contacts.between.controlled.catchments)

      # exclude within catchment movements from the matrix of contacts made to other sites within controlled catchments
      contacts.between.controlled.catchments <- contacts.between.controlled.catchments - contacts.withinCatchment.by.controlledSites

      # create matrix of all contacts made outside of the infection area (rather than outside of each individual catchment)
      matrix_contacts_exclude <- matrix_contacts_exclude - contacts.between.controlled.catchments

    # else if the site control type is 2 (allow no movements by any site within an infected catchment)
    } else if (site_control_type == 2) {
      # assign contact probability matrix for sites within controlled catchments as matrix_contacts_exclude
      matrix_contacts_exclude <- contacts.by.controlledSites
    }
  }

  # create matrix of contacts to remove and remove from input matrix of risk contacts
  atriskcontacts.toremove <- spmatrix_risk_contacts * matrix_contacts_exclude
  spmatrix_risk_contacts <- spmatrix_risk_contacts - atriskcontacts.toremove

  # reassign new catchment control information (catchment, contacts to exclude and number of catchments)
  catchment_movements[[3]] <- catchments_controlled
  catchment_movements[[4]] <- matrix_contacts_exclude
  catchment_movements[[7]] <- n_catchments_controlled

  # return list containing (1) sparse matrix of risk contacts (excluding within catchment movements
  # depending on site control measures) and (2) catchment_movements list with updated controlled catchments
  return(list(spmatrix_risk_contacts, catchment_movements))
}
