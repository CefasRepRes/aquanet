#' calcRandomSpillover
#'
#' Generate a list of transition rate objects using an input vector of clinically infected sites
#' (`clinical_state_vector`) to extract sites that are susecptible to infection that do not have
#' restricted movements on site. Additionally, identify sites with spreading potential that are
#' clinically infected and have no restrictions on movements off site.
#'
#' Create list of vectors with length equivalent to the number of sites with spreading potential and
#'  containing: (1) transition type vector depicted by the column number of `trans_name` within the
#' `run_time_params`, (2) randomly selected transitioning/susceptible site vector, (3) transition
#' rate/probability vector calculated using `1 / run_time_params[[trans_name]]`, and (4) source site
#'  vector of NAs. This list is populated with NULL if there are no susceptible sites. If there are
#' 1 or more susceptible sites, the fifth element of this list is the number of infected sites.
#'
#' Note: in aquanet-mod, this function is called multiple times exclusively within the `update_rate`
#' function.
#'
#' TODO: update function names if changed
#'
#' @param clinical_state_vector  (class numeric) numeric binary vector of length number of sites
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
#' @param site_indices (class numeric) vector of 0-based site indices of length 'number of sites'.
#'
#' @param trans_name (class string) string in quotation marks "" stating transition type. Note: this
#'  should correspond to a column name in the input parameter file and is case sensitive. An example
#' input may be "Fomite_Transmission_Independent_Prob".
#'
#' @param run_time_params (class data frame) of model run time parameters imported from original
#' parameter file which is subsequently split into model set up and model run time parameters. Data
#' frame contains probabilities from the scenarios of interest for listing transition rates.
#'
#' @param trans_type (class numeric) single numeric value indicating the type of transition occurring.
#'
#' @return (class list) of either length 4 or 5 containing:
#' 1. (class numeric) numeric vector of transition types (`trans_type`) with length 'number of sites
#' that are infected with no restrictions on movement off site'.
#'
#' 2. (class integer) integer vector of 0-based indices of site subject to transition i.e.
#' susceptible (positions/coordinate x of site randomly sampled from susceptible sites with no
#' restrictions to prevent spread on site) with length 'number of sites that are infected with no
#' restrictions on movement off site'.
#'
#' 3. (class numeric) numeric vector of transition rate/transmission probability (the probability
#' of risk contact at sites connected to susceptible sites) calculated by
#' `1 / run_time_params[[trans_name]]` with length 'number of sites that are infected with no
#' resrictions on movement off site'.
#'
#' 4. (class logical) logical vector containing NA's to depict unknown infection source with length
#' 'number of sites that are infected with no movement off site'.
#'
#' 5. (class integer) number of sites that are infected with no restrictions on movements off site.
#'
#' @export
#'
calcRandomSpillover <- function(clinical_state_vector,
                                spread_restricted_off,
                                spread_restricted_on,
                                site_indices,
                                trans_name,
                                run_time_params,
                                trans_type) {
  # create vector of susceptible site IDs with no restricted spread on site
  sites_susceptible <- site_indices[!clinical_state_vector & !spread_restricted_on]
  n_sites_S <- length(sites_susceptible)

  # create vector of infected site IDs with no resticted spread off site
  sites_spreading_I <- site_indices[clinical_state_vector & !spread_restricted_off]
  n_sites_I <- length(sites_spreading_I)

  # calculate tranmission probability
  prob <- 1 / run_time_params[[trans_name]]

  # if there are susceptible sites:
  if (n_sites_S != 0) {
    # take a sample of one susceptible site
    site <- sample.int(n_sites_S, size = 1, replace = TRUE)

    # create populated output list
    list_infection_rates <- list(rep.int(trans_type, times = n_sites_I),
                                 rep.int(sites_susceptible[site], times = n_sites_I),
                                 rep.int(prob, times = n_sites_I),
                                 rep.int(NA, times = n_sites_I),
                                 n_sites_I)

  # if there are no susceptible sites:
  } else {
    # create empty output list
    list_infection_rates <- list(NULL, NULL, NULL, NULL)
  }

  return(list_infection_rates)
}
