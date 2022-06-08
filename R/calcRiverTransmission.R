#' calcRiverTransmission
#'
#' Take the river transmission probability matrix and correct source to receiving site transmission
#' probabilities so only connections where the source site is clinically infected
#' (`state_vector = 1`) and where movements off site are NOT prevented (i.e. movements off site are
#' allowed) have a transmission probability greater than 0. In addition. correct receiving from
#' source site transmission probabilities so only connections where the receiving site has NO
#' restrictions on on site movements.
#'
#' Use the corrected river transmission probabilities matrix to extract a list of information
#' related to the sites that have the potential to be infected using
#' `aquanet::listRatesSusceptibleRiskContacts()`.
#'
#' TODO: update function names if changed (listRatesSusceptibleRiskContacts/listInfectionRates)
#'
#' @param matrix_river_distances_prob (class dgTmMatrix, Matrix package) sparse matrix containing
#' probability of transmission between sites connected via the river network by river water. Note:
#' this is the second element in the list output of
#' `aquanet::createRiverDistanceProbabilityMatrix()`.
#'
#' @param clinical_state_vector (class numeric) numeric binary vector of length number of sites
#' containing information on whether each site is in an 1 = clinically infected or 0 =
#' non-clinically infected state (the latter includes both susceptible and latently infected sites).
#' Note: vector created within the `update_rate` function.
#'
#' @param spread_restricted_off (class logical) logical vector of length number of sites that
#' states whether movements off this site are currently restricted (TRUE) or unrestricted (FALSE).
#' (Note: created in the `update_rate` function of aquanet-mod).
#'
#' @param spread_restricted_on (class logical) logical vector of length number of sites that
#' states whether movements on to this site are currently restricted (TRUE) or unrestricted (FALSE).
#' (Note: created in the `update_rate` function of aquanet-mod).
#'
#' @param trans_type (class numeric) number stating transition type.
#'
#'
#' @return (class list) of length 5 from `aquanet::listRatesSusceptibleRiskContacts` containing:
#' 1. (class numeric) numeric vector of transition types (input `trans_type`) with length 'number of
#'  sites with contact probability greater than 0 where the receiving site is in a susceptible state'.
#'
#' 2. (class integer) integer vector of sites subject to transition (positions/coordinate x of sites
#'  with risk contact at susceptible sites `(spmatrix_risk_contacts@i)`) with length 'number of
#'  sites with contact probability greater than 0 where the receiving site is in a susceptible state'.
#'
#' 3. (class numeric) numeric vector of transition rates/transmission probability (the probability
#' of risk contact at sites connected to susceptible sites. `(spmatrix_risk_contacts@x)`) with
#' length 'number of sites with contact probability greater than 0 where the receiving site is in a
#' susceptible state'.
#'
#' 4. (class integer) numeric vector of source sites (positions/coordinate y of sites with risk
#' contact at susceptible sites `(spmatrix_risk_contacts@j)`) with length 'number of sites with
#' contact probability greater than 0 where the receiving site is in a susceptible state'.
#'
#' 5. (class integer) number of sites that have a risk contact probability above 0 where the
#' receiving site is in a susceptible state.
#'
#' @export
#'
calcRiverTransmission <- function(matrix_river_distances_prob,
                                  clinical_state_vector,
                                  spread_restricted_off,
                                  spread_restricted_on,
                                  trans_type) {

  # multiply probability matrix by clinical sites where offsite movements are not restricted
  matrix_river_distances_prob <- matrix_river_distances_prob * (clinical_state_vector * !spread_restricted_off)

  # transpose and multiply probability matrix by sites where onsite movements are not restricted
  matrix_river_distances_prob <- t(matrix_river_distances_prob) * !spread_restricted_on

  # re-transpose probability matrix
  matrix_river_distances_prob <- t(matrix_river_distances_prob)

  # calculate infection rate for river transmission
  list_river_infection_rates <- aquanet::listRatesSusceptibleRiskContacts(spmatrix_risk_contacts = matrix_river_distances_prob,
                                                                          state_vector = clinical_state_vector,
                                                                          trans_type = trans_type)

  return(list_river_infection_rates)
}
