excludeWithinCatchmentMovements = function(movement.restrictions.bySite, atriskcontacts, withinCatchmentMovements.objects) {
  graph.catchment2site.matrix2 = withinCatchmentMovements.objects[[1]]
  graph.withinCatchmentEdges.matrix = withinCatchmentMovements.objects[[2]]
  controlled.catchments.previous = withinCatchmentMovements.objects[[3]]
  listContacts.exclude = withinCatchmentMovements.objects[[4]]
  associatedSiteControlType = withinCatchmentMovements.objects[[5]]

  # Identify catchments placed under control, based on the list of sites currently under control
  controlled.catchments = t(graph.catchment2site.matrix2) %*% movement.restrictions.bySite
  no.controlled.catchments = sum(controlled.catchments > 0)

  # If the same catchments are under control as the last time the function was called, skip several steps
  if ((!all(controlled.catchments@x == controlled.catchments.previous@x)) == TRUE) {
    # Lookup a list of all of the sites contained within the controlled catchments
    secondary.controlled.sites = as.vector(graph.catchment2site.matrix2 %*% controlled.catchments)
    secondary.controlled.sites[secondary.controlled.sites > 1] = 1

    # List all of the contacts made by sites within controlled catchments
    contacts.by.controlledSites = contactp * secondary.controlled.sites
    contacts.by.controlledSites[contacts.by.controlledSites > 0] = 1

    # Store the list of sites that are under catchment level controls
    withinCatchmentMovements.objects[[6]] = secondary.controlled.sites

    if (associatedSiteControlType %in% c(0,1)) {
      # List all of the contacts made within controlled catchments
      contacts.withinCatchment.by.controlledSites = contacts.by.controlledSites * graph.withinCatchmentEdges.matrix

      # Identify all of the contacts made outside of controlled catchments
      listContacts.exclude = contacts.by.controlledSites - contacts.withinCatchment.by.controlledSites
    }

    if (associatedSiteControlType == 1) {
      # Identify contacts made to other sites within controlled catchments
      contacts.between.controlled.catchments = t(contacts.by.controlledSites) * secondary.controlled.sites
      contacts.between.controlled.catchments = t(contacts.between.controlled.catchments)
      # Exclude within catchment movements, from the list of contacts made to other sites within controlled catchments
      contacts.between.controlled.catchments = contacts.between.controlled.catchments - contacts.withinCatchment.by.controlledSites
      # Identify all of the contacts made outside of the infection area, rather than outside of each individual catchment
      listContacts.exclude = listContacts.exclude - contacts.between.controlled.catchments
    } else if (associatedSiteControlType == 2) {
      listContacts.exclude = contacts.by.controlledSites
    }
  }

  atriskcontacts.toremove = atriskcontacts * listContacts.exclude
  atriskcontacts = atriskcontacts - atriskcontacts.toremove

  withinCatchmentMovements.objects[[3]] = controlled.catchments
  withinCatchmentMovements.objects[[4]] = listContacts.exclude
  withinCatchmentMovements.objects[[7]] = no.controlled.catchments

  return(list(atriskcontacts, withinCatchmentMovements.objects))
}