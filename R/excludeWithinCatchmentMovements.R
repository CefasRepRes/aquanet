excludeWithinCatchmentMovements <- function(movement.restrictions.bySite, atriskcontacts, catchment_movements) {
  # extract elements from list
  graph.catchment2site.matrix2 <- catchment_movements[[1]]
  graph.withinCatchmentEdges.matrix <- catchment_movements[[2]]
  controlled.catchments.previous <- catchment_movements[[3]]
  listContacts.exclude <- catchment_movements[[4]]
  associatedSiteControlType <- catchment_movements[[5]]

  # Identify catchments placed under control, based on the list of sites currently under control
  catchments_controlled <- t(graph.catchment2site.matrix2) %*% movement.restrictions.bySite
  no.controlled.catchments <- sum(catchments_controlled > 0)

  # If the same catchments are under control as the last time the function was called, skip several steps
  if (!all(catchments_controlled@x == controlled.catchments.previous@x)) {
    # Lookup a list of all of the sites contained within the controlled catchments
    secondary.controlled.sites <- as.vector(graph.catchment2site.matrix2 %*% catchments_controlled)
    secondary.controlled.sites[secondary.controlled.sites > 1] <- 1

    # List all of the contacts made by sites within controlled catchments
    contacts.by.controlledSites <- contactp * secondary.controlled.sites
    contacts.by.controlledSites[contacts.by.controlledSites > 0] <- 1

    # Store the list of sites that are under catchment level controls
    catchment_movements[[6]] <- secondary.controlled.sites

    if (associatedSiteControlType %in% c(0,1)) {
      # List all of the contacts made within controlled catchments
      contacts.withinCatchment.by.controlledSites <- contacts.by.controlledSites * graph.withinCatchmentEdges.matrix

      # Identify all of the contacts made outside of controlled catchments
      listContacts.exclude <- contacts.by.controlledSites - contacts.withinCatchment.by.controlledSites
    }

    if (associatedSiteControlType == 1) {
      # Identify contacts made to other sites within controlled catchments
      contacts.between.controlled.catchments <- t(contacts.by.controlledSites) * secondary.controlled.sites
      contacts.between.controlled.catchments <- t(contacts.between.controlled.catchments)
      # Exclude within catchment movements, from the list of contacts made to other sites within controlled catchments
      contacts.between.controlled.catchments <- contacts.between.controlled.catchments - contacts.withinCatchment.by.controlledSites
      # Identify all of the contacts made outside of the infection area, rather than outside of each individual catchment
      listContacts.exclude <- listContacts.exclude - contacts.between.controlled.catchments
    } else if (associatedSiteControlType == 2) {
      listContacts.exclude <- contacts.by.controlledSites
    }
  }

  atriskcontacts.toremove <- atriskcontacts * listContacts.exclude
  atriskcontacts <- atriskcontacts - atriskcontacts.toremove

  catchment_movements[[3]] <- catchments_controlled
  catchment_movements[[4]] <- listContacts.exclude
  catchment_movements[[7]] <- no.controlled.catchments

  return(list(atriskcontacts, catchment_movements))
}
