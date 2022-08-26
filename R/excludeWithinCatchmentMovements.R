#' excludeWithinCatchmentMovements
#'
#' This function removes contacts between sites within the same catchment from the transmission
#' probabilities via LFM route of AquaNet-Mod (see details).
#'
#' Identify the catchments that are under movement controls based on the list of sites with movement
#'  restrictions `move_restricted_sites` and determine total number of catchments under controls.
#'
#' If the catchments under control are the same as the previous time step then subtract the
#' previously defined matrix of risk contacts `matrix_contacts_exclude` in
#' `catchment_movements[[4]]` containing risk contacts that should be excluded due to movement
#' restrictions from the input matrix of risk contacts `spmatrix_risk_contacts`.
#'
#' If the catchments under control in the current time step differ from the previous time step then
#' extract a vector of sites that are contained within controlled catchments and reassign this to
#' `catchment_movements[[6]]`. Use this list of sites to produce a matrix of contact probabilities
#' for sites within controlled catchments. Then, depending on the level of catchment controls
#' defined in `catchment_movements[[5]]` generate a matrix of within catchment site to site contacts
#'  to exclude.
#'
#' In both scenarios subtract the matrix of contacts to exclude from the input matrix of risk
#' contacts `spmatrix_risk_contacts`. Then reassign objects 3, 4 and 7 within the
#' `catchment_movements` list to update: (3) the matrix of controlled catchments, (4) the matrix of
#' contacts that are excluded based on the catchment controls in place and (7) the total number of
#' controlled catchments.
#'
#' @param move_restricted_sites (class logical) logical vector of length 'number of sites' that
#' states whether movements at this site are currently restricted (TRUE) or unrestricted (FALSE).
#' (Note: created in the `aquanet::updateRates` function of aquanet-mod).
#'
#' @param spmatrix_risk_contacts (class dgCMatrix, Matrix package) sparse matrix containing live
#' fish movements contact probability adapted to identify only contacts between sites that present a
#'  risk of disease spread. Source sites that are infected but cannot transport fish off site due
#' to movement restrictions have their contact probabilities converted to 0 as they cannot form
#' 'at risk' contacts. Additionally. sites that are uninfected with or without movement restrictions
#' have a probability of 0. Receiving sites that cannot transport fish on site due to movement
#' restrictions also have their contact probabilities converted to 0 as they cannot form 'at risk'
#' contacts. At risk contacts occur between sites that are infected with no restrictions on movement
#'  off site and receiving sites with no restrictions on site. (Note: defined within
#' `aquanet::updateRates` function of aquanet-mod).
#'
#' @param catchment_movements (class list) of length 7, containing objects related to catchment-
#' level movements:
#' 1. (class dgCMatrix, Matrix package) sparse matrix containing details of site to catchment
#' relationships.
#' 2. (class lgCMatrix, Matrix package) logical matrix contacting details of sites within the same
#' catchment.
#' 3. (class dgeMatrix, Matrix package) matrix of length number of catchments showing which
#' catchments were under controls in the previous time step.
#' 4. (class dgTMatrix, Matrix package) sparse matrix containing contacts to exclude.
#' 5. (class numeric) number selecting catchment level controls to apply (0 = allows movements
#' within the same catchments, 1 = allows movements within or between infected catchments, and 2 =
#' allows no movements by any of the sites within an infected catchment, "None" means there are no
#' catchment level controls).
#' 6. (class logical) logical vector of length number of sites stating if sites are under secondary
#'  levels of control.
#' 7. (class numeric) the total number of catchments under controls.
#'
#' @param matrix_movements_prob (class dgTMatrix, Matrix package) sparse matrix containing the
#' probability of live fish movements between sites. (Note: output of
#' `createContactProbabilityMatrix` function of aquanet-mod).
#'
#' @return (class list) of length 2 containing:
#' 1. (dgCMatrix, Matrix package) sparse matrix of corrected 'at risk' contacts.
#' 2. (class list) of length 7 containing: updated catchment_movements input. Updated elements
#' include 3, 4, 6, and 7 (element 6 only changes if different catchments are under control compared
#' to previous time step).
#'
#' @export
#'
#' @importFrom Matrix t
excludeWithinCatchmentMovements <- function(move_restricted_sites,
                                            spmatrix_risk_contacts,
                                            catchment_movements,
                                            matrix_movements_prob) {
  # extract elements from list
  spmatrix_sites_catchment <- catchment_movements[[1]]
  lgmatrix_catch_catch <- catchment_movements[[2]]
  catchments_controlled_prev <- catchment_movements[[3]]
  matrix_contacts_exclude <- catchment_movements[[4]]
  site_control_type <- catchment_movements[[5]]

  # create matrix of catchments (rows) under control (col 1) by multiplying the sites by whether
  # movements are restricted if there are no catchment controls, ignore this step
  ifelse(site_control_type != "None",
  catchments_controlled <- Matrix::t(spmatrix_sites_catchment) %*% move_restricted_sites,
  catchments_controlled <- Matrix::t(spmatrix_sites_catchment) %*% move_restricted_sites * 0)

  # determine number of catchments under control
  n_catchments_controlled <- sum(catchments_controlled > 0)

  # if the catchments under control are NOT the same as last time step:
  if (!all(catchments_controlled@x == catchments_controlled_prev@x)) {
    # return vector of sites contained within the controlled catchments and correct those over 1
    sites_controlled <- as.vector(spmatrix_sites_catchment %*% catchments_controlled)
    sites_controlled[sites_controlled > 1] <- 1

    # create contact probability matrix for sites within controlled catchments
    # sites outside controlled catchments have p = 0 else 1
    sites_controlled_prob <- matrix_movements_prob * sites_controlled
    sites_controlled_prob[sites_controlled_prob > 0] <- 1

    # reassign the secondary controlled sites element with sites under catchment controls in this time step
    catchment_movements[[6]] <- sites_controlled

    # if the site control type is 0 or 1
    if (site_control_type %in% c(0,1)) {
      # create matrix of all contacts made within controlled catchments
      sites_controlled_in_catchment_prob <- sites_controlled_prob * lgmatrix_catch_catch

      # create matrix of all contacts made outside of controlled catchments
      matrix_contacts_exclude <- sites_controlled_prob - sites_controlled_in_catchment_prob
    }

    # if the site control type is 1 (allow movements within or between infected catchments)
    if (site_control_type == 1) {
      # create matrix of contacts made to other sites within controlled catchments
      sites_controlled_between_catchment_prob <- Matrix::t(sites_controlled_prob) * sites_controlled
      sites_controlled_between_catchment_prob <- Matrix::t(sites_controlled_between_catchment_prob)

      # exclude within catchment movements from the matrix of contacts made to other sites within
      # controlled catchments
      sites_controlled_between_catchment_prob <-
        sites_controlled_between_catchment_prob - sites_controlled_in_catchment_prob

      # create matrix of all contacts made outside of the infection area
      # (rather than outside of each individual catchment)
      matrix_contacts_exclude <- matrix_contacts_exclude - sites_controlled_between_catchment_prob

    # else if the site control type is 2 (allow no movements by any site within an infected catchment)
    } else if (site_control_type == 2) {
      # assign contact probability matrix for sites within controlled catchments as matrix_contacts_exclude
      matrix_contacts_exclude <- sites_controlled_prob
    }
  }

  # create matrix of contacts to remove and remove from input matrix of risk contacts
  risk_contacts_remove <- spmatrix_risk_contacts * matrix_contacts_exclude
  spmatrix_risk_contacts <- spmatrix_risk_contacts - risk_contacts_remove

  # reassign new catchment control information (catchment, contacts to exclude and number of catchments)
  catchment_movements[[3]] <- catchments_controlled
  catchment_movements[[4]] <- matrix_contacts_exclude
  catchment_movements[[7]] <- n_catchments_controlled

  # return list containing (1) sparse matrix of risk contacts (excluding within catchment movements
  # depending on site control measures) and (2) catchment_movements list with updated controlled catchments
  return(list(spmatrix_risk_contacts, catchment_movements))
}
