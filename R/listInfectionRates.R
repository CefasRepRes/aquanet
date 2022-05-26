#' listRatesSusceptibleRiskContacts
#'
#' Extract information from risk contact matrix related to the sites that have the potential to be
#' infected (i.e. contact probability is greater than 0 and receiving sites are in a susceptible
#' state `state_vector[site] == 0`). Output information includes receiving siteID, probability of
#' contact, source siteID, and the number of contacts that pose a risk to receiving sites. A
#' transition rate vector is also produced for these sites using the `trans_type` input parameter.
#'
#' Note: this function is called within `update_rate` and `calcRiverTransmission` of aquanet-mod.
#'
#' TODO: CHECK/UPDATE FUNCTION NAMES
#'
#' @param spmatrix_risk_contacts  (class dgCMatrix, Matrix package) sparse matrix containing live
#' fish movements contact probability adapted to identify only contacts between sites that present a
#'  risk of spread (of e.g. a pathogen). In aquanet-mod movements between sites in the same
#' catchment are filtered depending on catchment controls input into
#' `excludeWithinCatchmentMovements()`.
#'
#' Source sites that are infected but cannot transport fish off site due to movement restrictions
#' have their contact probabilities converted to 0 as they cannot form 'at risk' contacts.
#' Additionally. sites that are uninfected with or without movement restrictions have a probability
#' of 0. Receiving sites that cannot transport fish on site due to movement restrictions also have
#' their contact probabilities converted to 0 as they cannot form 'at risk' contacts. At risk
#' contacts occur between sites that are infected with no restrictions on movement off site and
#' receiving sites with no restrictions on site.
#' (Note: initially defined within `update_rate` function of aquanet-mod. In aquanet-mod this is
#' one of the outputs from excludeWithinCatchmentMovements()).
#'
#' @param state_vector (class numeric) numeric binary vector of length number of sites containing
#' information on whether each site is in an 1 = infected or 0 = susceptible state.
#' (Note: created within the `simulationCode` function for loop)
#'
#' @param trans_type (class numeric) number stating transition type.
#'
#' @return (class list) of length 5 containing:
#' 1. (class numeric) numeric vector of input `trans_type` with length 'number of sites with contact
#'  probability greater than 0 where the receiving site is in a susceptible state'.
#'
#' 2. (class integer) integer vector containing the positions/coordinate x of sites with risk
#' contact at susceptible sites `(spmatrix_risk_contacts@i)` with length 'number of sites with
#' contact probability greater than 0 where the receiving site is in a susceptible state'.
#'
#' 3. (class numeric) numeric vector containing the probability of risk contact at sites connected
#' to susceptible sites. `(spmatrix_risk_contacts@x)` with length 'number of sites with contact
#' probability greater than 0 where the receiving site is in a susceptible state'.
#'
#' 4. (class integer) numeric vector containing the positions/coordinate y of sites with risk
#' contact at susceptible sites. These are the source sites `(spmatrix_risk_contacts@j)` with length
#'  'number of sites with contact probability greater than 0 where the receiving site is in a
#'  susceptible state'.
#'
#' 5. (class integer) number of sites that have a risk contact probability above 0 where the
#' receiving site is in a susceptible state.
#'
#' @export
#'
#' @importFrom stats setNames
#' @importFrom methods slot
#'
listRatesSusceptibleRiskContacts <- function(spmatrix_risk_contacts, state_vector, trans_type) {
  # convert 'dgCMatrix' to 'dgTMatrix'
  spmatrix_risk_contacts <- as(t(spmatrix_risk_contacts), "dgTMatrix")

  # get matrix value coordinates (i, j) and values (x)
  # NOTE: 0-based coordinates (most R objects have 1-based coordinates)
  slots <- c("i", "j", "x")
  slots_risk_contacts <- lapply(setNames(slots, slots), FUN = function(x) { slot(spmatrix_risk_contacts, x) })

  # create a logical vector stating whether x is non-zero
  non_zero <- slots_risk_contacts[["x"]] != 0

  # extract coordinates (i, j) and values (x) for at risk contact probabilities which are non-zero
  slots_risk_contacts <- lapply(setNames(slots, slots), FUN = function(x) { slots_risk_contacts[[x]][non_zero]})

  # create logical vector of edges with potential to infect susceptible sites NOTE: i + 1 as 0-based coordinates
  edge_susceptible <- state_vector[slots_risk_contacts[["i"]] + 1] == 0

  # subset risk contacts leading to susceptible sites
  contacts <- lapply(setNames(slots, c("position", "source", "rate")),
                     FUN = function(x) { slots_risk_contacts[[x]][edge_susceptible]})

  # determine number of contacts
  n_contacts <- length(contacts[["rate"]])
  contacts_rate_type <- rep.int(trans_type, times = n_contacts)

  return(list(contacts_rate_type, contacts[["position"]], contacts[["rate"]], contacts[["source"]], n_contacts))
}
