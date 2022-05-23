excludeWithinCatchmentMovements <- function(movement.restrictions.bySite, spmatrix_risk_contacts, catchment_movements) {
  # TODO: replace list numbers with named elements
  # extract elements from list
  spmatrix_sites_catchment <- catchment_movements[[1]]
  lgmatrix_catch_catch <- catchment_movements[[2]]
  catchments_controlled_prev <- catchment_movements[[3]]
  matrix_contacts_exclude <- catchment_movements[[4]]
  site_control_type <- catchment_movements[[5]]

  # Identify catchments placed under control, based on the list of sites currently under control
  catchments_controlled <- t(spmatrix_sites_catchment) %*% movement.restrictions.bySite
  n_catchments_controlled <- sum(catchments_controlled > 0)

  # If the same catchments are under control as the last time the function was called, skip several steps
  if (!all(catchments_controlled@x == catchments_controlled_prev@x)) {
    # Lookup a list of all of the sites contained within the controlled catchments
    secondary.controlled.sites <- as.vector(spmatrix_sites_catchment %*% catchments_controlled)
    secondary.controlled.sites[secondary.controlled.sites > 1] <- 1

    # List all of the contacts made by sites within controlled catchments
    contacts.by.controlledSites <- contactp * secondary.controlled.sites
    contacts.by.controlledSites[contacts.by.controlledSites > 0] <- 1

    # Store the list of sites that are under catchment level controls
    catchment_movements[[6]] <- secondary.controlled.sites

    if (site_control_type %in% c(0,1)) {
      # List all of the contacts made within controlled catchments
      contacts.withinCatchment.by.controlledSites <- contacts.by.controlledSites * lgmatrix_catch_catch

      # Identify all of the contacts made outside of controlled catchments
      matrix_contacts_exclude <- contacts.by.controlledSites - contacts.withinCatchment.by.controlledSites
    }

    if (site_control_type == 1) {
      # Identify contacts made to other sites within controlled catchments
      contacts.between.controlled.catchments <- t(contacts.by.controlledSites) * secondary.controlled.sites
      contacts.between.controlled.catchments <- t(contacts.between.controlled.catchments)
      # Exclude within catchment movements, from the list of contacts made to other sites within controlled catchments
      contacts.between.controlled.catchments <- contacts.between.controlled.catchments - contacts.withinCatchment.by.controlledSites
      # Identify all of the contacts made outside of the infection area, rather than outside of each individual catchment
      matrix_contacts_exclude <- matrix_contacts_exclude - contacts.between.controlled.catchments
    } else if (site_control_type == 2) {
      matrix_contacts_exclude <- contacts.by.controlledSites
    }
  }

  atriskcontacts.toremove <- spmatrix_risk_contacts * matrix_contacts_exclude
  spmatrix_risk_contacts <- spmatrix_risk_contacts - atriskcontacts.toremove

  catchment_movements[[3]] <- catchments_controlled
  catchment_movements[[4]] <- matrix_contacts_exclude
  catchment_movements[[7]] <- n_catchments_controlled

  return(list(spmatrix_risk_contacts, catchment_movements))
}
