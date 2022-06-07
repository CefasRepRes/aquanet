#' listTransitionRates
#'
#' Generate a list of transition rate objects using an input vector of sites stating their infection
#' status (`state_vector`) to extract sites (from `site_indices`) with a specified infection status
#' (susceptible/infected) `infection_state`). For these sites, generate a vector of the transition
#' rate: `1 / run_time_params[[trans_type]]`. The column number of `trans_type` within the
#' `run_time_params` depicts the rate type for the site - this is output in a list alongside the
#' transition rate, transitioning site, rate, source site (if available), and number of sites.
#'
#' Note: in aquanet-mod, this function is called multiple times exclusively within the `update_rate`
#' function.
#'
#' @param run_time_params (class data frame) of model run time parameters imported from original
#' parameter file which is subsequently split into model set up and model run time parameters. Data
#' frame contains probabilities from the scenarios of interest for listing transition rates.
#'
#' @param state_vector (class numeric) numeric binary vector of length number of sites containing
#' information on whether each site is in an 1 = infected or 0 = susceptible state.
#'
#' @param trans_type (class string) string in quotation marks "" stating transition type. Note: this
#'  should correspond to a column name in the input parameter file and is case sensitive. An example
#'  input may be "Site_Recovers".
#'
#' @param site_indices (class numeric) vector of 0-based site indices of length 'number of sites'.
#'
#' @param infection_state (class numeric) infection status of sites whose transition rates should be
#'  determined to subset either 1 = infected or 0 = susceptible.
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
listTransitionRates <- function(run_time_params, state_vector, trans_type, site_indices,
                                infection_state) {
  # get column number for trans_type in run_time_params
  trans_num <- which(colnames(run_time_params) == trans_type)

  # get probability from input parameter file
  prob <- run_time_params[[trans_type]]

  # create logical vector of sites in input infection_state specified
  state_logical <- state_vector == infection_state

  # get the position and total number of sites in input infection_state specified
  position <- site_indices[state_logical]
  n_rates <- length(position)

  # create vector of transition rates, transition rate type, and infection source
  rate <- rep(1 / prob, times = n_rates)
  rate_type <- rep(trans_num, times = n_rates)
  source_site <- rep(NA, times = n_rates)

  return(list(rate_type, position, rate, source_site, n_rates))
}
