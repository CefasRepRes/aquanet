#' listTransitionRates
#'
#' This function lists transition rates for all site state transition probabilities except for
#' disease transmission via LFM route of AquaNet-Mod (see details).
#'
#' Generate a list of transition rate objects using an input vector of sites stating their status
#' (`state_vector`) (for example infection status 1/0, fallow status, latency status) to extract
#' sites (from `site_indices`) where the state in state_vector is 1. For these sites, generate a
#' vector of the transition rate: `1 / run_time_params[[trans_name]]`. The column number of
#' `trans_name` within the `run_time_params` depicts the rate type for the site - this is output in
#' a list alongside the transition rate, transitioning site, rate, source site (NAs), and number of
#' sites.
#'
#' Note: in AquaNet-Mod, this function is called multiple times exclusively within the
#' `aquanet::updateRates` function.
#'
#' @param run_time_params (class data frame) of model run time parameters imported from original
#' parameter file which is subsequently split into model set up and model run time parameters. Data
#' frame contains probabilities from the scenarios of interest for listing transition rates.
#'
#' @param state_vector (class numeric) numeric binary vector of length number of sites containing
#' information about the state of each site in relation to a condition (e.g. is the site
#' 1 = infected or 0 = susceptible state).
#'
#' @param trans_name (class string) string in quotation marks "" stating transition type. Note: this
#'  should correspond to a column name in the input parameter file and is case sensitive. An example
#'  input may be "Site_Recovers".
#'
#' @param site_indices (class numeric) vector of 0-based site indices of length 'number of sites'.
#'
#' @param trans_type (class numeric) single numeric value indicating the type of transition occurring.
#'
#' @return (class list) of length 5 containing:
#' 1. (class numeric) numeric vector of transition types (`rate_type`).
#' 2. (class integer) integer vector of sites subject to transition (`position`).
#' 3. (class numeric) numeric vector of transition rates (`rate`).
#' 4. (class logical) logical vector of NA's for infection source (`source_site`).
#' 5. (class integer) number of sites in listed transition rates (`n_rates`).
#'
#' @export
#'
listTransitionRates <- function(run_time_params,
                                state_vector,
                                trans_name,
                                site_indices,
                                trans_type) {

  # get probability from input parameter file
  prob <- run_time_params[[trans_name]]

  # create logical vector of sites in state_vector state 1
  state_logical <- state_vector == 1

  # get the position and total number of sites in state 1
  position <- site_indices[state_logical]
  n_rates <- length(position)

  # create vector of transition rates, transition rate type, and infection source
  rate <- rep(1 / prob, times = n_rates)
  rate_type <- rep(trans_type, times = n_rates)
  source_site <- rep(NA, times = n_rates)

  return(list(rate_type = rate_type,
              position = position,
              rate = rate,
              source_site = source_site,
              n_rates = n_rates))
}
