#' updateRates
#'
#' This function ,,, (see details).
#'
#' Define logical vectors of sites depending on different combinations of  movement restrictions and
#'  site states specified within an input `control_matrix`. This includes sites where movements are
#' restricted, where transport of animals on and off site is restricted, and sites where spread on
#' and off site are not possible due to restrictions.
#'
#' Next, vectors of different site types are defined based on site states, site types, infection
#' status and the ability to cull to achieve recovered status at sites. Using this information, live
#'  fish movements that have the potential to spread (e.g. a pathogen) are identified, and within
#' catchment movements are corrected to remove within catchment movements.
#'
#' Epidemic rates are then calculated, including: site infection rate, farm recovery rate, fishery
#' latency rate, site clearing rate, site disinfection rate, site contact tracing rate, site
#' detection rate, farm fallow rate. These are combined and output.
#'
#' If the current time step is within the peak transmission season specified within the inputs then
#' the rate at which latent sites become reinfected, the rate at which spread occurs via river
#' network connectivity and the rate of fomite transmission are calculated and also combined.
#'
#' Finally, if there are susceptible sites without restrictions preventing spread on site, a rate of
#'  random spillover into these sites is also calculated and combined with the outputs.
#'
#' @param control_matrix (class matrix) matrix containing 7 columns depicting different control
#' states and rows depicting whether each sites is 1 = in the specified control state or 0 = not in
#' the specified control state.
#'
#' @param state_vector (class numeric)  numeric binary vector of length 'number of sites' containing
#' information about the state of each site in relation to a condition. This state vector should
#' specify whether a site is in an 1 = infected or 0 = susceptible state. (Note: created within the
#' `aquanet::simulationCode` function for loop).
#'
#' @param farm_vector (class numeric) numeric binary vector of length number of sites containing
#' information on whether each site 1 = is a farm or 0 = is not a farm.
#'
#' @param culling_vector (class numeric) numeric binary vector of length number of sites containing
#' information on whether each site 1 = can be culled or 0 = cannot be culled.
#'
#' @param site_indices (class numeric) vector of 0-based site indices of length 'number of sites'.
#'
#' @param catchment_movements (class list) of length 7 produced using the containing objects
#' related to catchment-level movements:
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
#' @param movements_prob (class dgTMatrix, Matrix package) a matrix of live fish movement
#' probabilities between sites generated using the `aquanet::createContactProbabilityMatrix()`
#' function.
#'
#' @param movements_prob_top_sites_removed (class dgTMatrix, Matrix package) a matrix of live fish
#' movement probabilities between sites, but with the top most connected sites in the network
#' removed. Generated using the `aquanet::createContactProbabilityMatrixTopSitesRemoved()`function.
#'
#' @param river_prob (class dgTMatrix, Matrix package) a matrix of river connectivity distance-based
#'  transmission probabilities generated using the `aquanet::createRiverDistanceProbabilityMatrix()`
#'  function.
#'
#' @param site_distances_prob (class dgTMatrix, Matrix package) a matrix of distance-based
#' transmission probabilities generated using the `aquanet::createDistanceMatrix()` function.
#'
#' @param run_time_params (class data frame) of model run time parameters imported from original
#' parameter file which is subsequently split into model set up and model run time parameters. Data
#' frame contains probabilities from the scenarios of interest for listing transition rates.
#'
#' @param non_peak_season (class logical) logical indicating whether the current time step in the
#' model is within non peak season where pathogen transmission is lower.
#'
#' @param contact_tracing (class logical) vector of length 1 indicating whether or not contact
#' tracing is taking place.
#'
#' @param remove_top_sites (class logical) vector of length 1 indicating whether or not the removal
#' of the most connected sites in the network should take place.
#'
#' @param remove_top_sites (class logical) vector of length 1 indicating whether or not the removal
#' of the most connected sites in the network should take place.
#'
#' @param sites_states_cumulative (class numeric) numeric binary vector indicating whether a site is
#' infected (1) or susceptible (0).
#'
#' @param n_infections_remove_top_sites (class numeric) vector of length 1. After the cumulative
#' number of infected sites exceeds this number, switch to using the top sites removed contact
#' probability matrix.
#'
#' @param disease_controls (class logical) vector of length 1 indicating whether or not any disease
#' control measures should take place.
#'
#' @return (class list) of length 3 containing:
#' 1. (class list) of length 4 containing transition rates:
#' 1.1. (class numeric) vector of transition types.
#' 1.2. (class integer) vector of sites subject to transition.
#' 1.3. (class integer) vector of transition rates (transmission probability).
#' 1.4. (class numeric) vector of source sites (of disease in case of transmission).
#'
#' 2. (class list) of length 7 containing movements_within_catchment input that has been updated #
#' with to `aquanet::excludeWithinCatchmentMovements()` function inside `aquanet::updateRates()`.
#' Updated elements include 3, 4, 6, and 7 (element 6 only changes if different catchments are under
#'  control compared to previous time step).
#'
#' 3. (class logical) logical vector of length number of sites containing information on whether a
#' site is subject to any kind of movement restrictions.
#'
#' @export
#'
#' @importFrom Matrix t
updateRates <- function(control_matrix,
                        state_vector,
                        farm_vector,
                        culling_vector,
                        site_indices,
                        catchment_movements,
                        movements_prob,
                        movements_prob_top_sites_removed,
                        river_prob,
                        site_distances_prob,
                        run_time_params,
                        non_peak_season,
                        contact_tracing,
                        remove_top_sites,
                        sites_states_cumulative,
                        n_infections_remove_top_sites,
                        disease_controls) {

  ### Select contact matrix ---

  if(remove_top_sites == TRUE && sum(sites_states_cumulative) > n_infections_remove_top_sites) {
    movement_probability <- movements_prob_top_sites_removed[["matrix_movements_prob"]]
  } else {
    movement_probability <- movements_prob[["matrix_movements_prob"]]
  }

  ### define movement restrictions ----

  # Scenario 1: culling - all sites in the surveillance and fallow periods
  # Note: for culling all sites transition from 2 -> 4
  # Note: when sites are contact traced, movements are controlled, but not culled until infection is confirmed
  # therefore contact tracing is not used to determine these movement restrictions
  sites_all_movement_restricted <- as.logical(control_matrix[ , c(2, 3, 4, 5)] %*% rep(1, 4))

  # Scenario 2: surveillance - sites in the surveillance period
  sites_movement_restricted <- as.logical(control_matrix[ , c(2, 3)] %*% rep(1, 2))

  # sites where movement is restricted on or off site (Note: latent sites can import but not export)
  # Note: include sites in surveillance stage 3 as they can move fish within infected catchments
  transport_prevented_on <- as.logical(control_matrix[ , c(2, 4, 5, 6, 7)] %*% rep(1, 5))
  transport_prevented_off <- as.logical(control_matrix[ , c(2, 4, 5, 7)] %*% rep(1, 4))

  # sites that have fallowed, are empty and can restock or are latently infected - i.e. cannot spread
  spread_prevented_on <- as.logical(control_matrix[ , c(4, 5, 6)] %*% rep(1, 3))
  spread_prevented_off <- spread_prevented_on


  ### define site types ----

  # sites that are NOT fallow, post-fallow state, or latent (i.e. not recovering)
  sites_I_recovery <- !as.logical(control_matrix[ , c(4, 5, 6)] %*% rep(1, 3))

  # create vector of infected farms that are NOT latent or fallow state (leading to recovery)
  farms_I <- state_vector * sites_I_recovery * farm_vector

  # create vector of infected fisheries that are NOT latent (or fallow state) (leading to latency)
  fisheries_I <- state_vector * sites_I_recovery * !farm_vector

  # create a vector of latently infected sites
  sites_L <- as.logical(control_matrix[ , 6])

  # create vector of sites that are infected and in the fallow or post-fallow state
  sites_fallow <- state_vector * (control_matrix[ , 4] + control_matrix[ , 5])

  # create vector of clinically infected sites
  clinical_vector <- state_vector * !control_matrix[ , 6]

  # NOTE only used if contact_tracing == TRUE
  # create vector of sites which have been contact traced (are in infected catchment)
  sites_contact_traced <- control_matrix[ , 7]

  if(disease_controls == TRUE){
  # create vector of sites that could be controlled (infection present and not detected)
  sites_I_undetected <- control_matrix[ , 1]

  # create vector of sites that can become fallow as they can be culled
  sites_I_controlled <- sites_movement_restricted * culling_vector
  }

  ### identify LFM contacts carrying risk ----

  # retain contact probabilities where origin site is infected with unrestricted transport off site
  matrix_risk_contacts <- movement_probability * (state_vector * !transport_prevented_off)

  # retain contact probabilities where receiving sites have no restricted transport on site
  matrix_risk_contacts <- Matrix::t(matrix_risk_contacts) * !transport_prevented_on
  matrix_risk_contacts <- Matrix::t(matrix_risk_contacts)

  # exclude within catchment movements unless there are no catchment movement restrictions
  risk_contacts_catch_corrected <- aquanet::excludeWithinCatchmentMovements(move_restricted_sites = sites_all_movement_restricted,
                                                                            spmatrix_risk_contacts = matrix_risk_contacts,
                                                                            catchment_movements = catchment_movements,
                                                                            matrix_movements_prob = movement_probability)

  ### calculate LFM infection rate ----

  # Rate 1: exposure rate of LFM contacts from infected to susceptible sites
  rate_site_infection <- aquanet::listRatesSusceptibleRiskContacts(spmatrix_risk_contacts = risk_contacts_catch_corrected[["spmatrix_risk_contacts"]],
                                                                   state_vector = state_vector,
                                                                   trans_type = 0)


  ### calculate transition rates ----

  # Rate 2: farm transitions from infected to subclinical infection
  rate_farm_recovery <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                     state_vector = farms_I,
                                                     trans_name = "Site_Recovers",
                                                     site_indices = site_indices,
                                                     trans_type = 3)

  # Rate 3: fishery transitions from infected to subclinical infection
  rate_fishery_latency <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = fisheries_I,
                                                       trans_name = "Infection_Becomes_Subclinical",
                                                       site_indices = site_indices,
                                                       trans_type = 2)

  # Rate 4: transition from subclinical infection (farms and fisheries)
  rate_site_cleared <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                    state_vector = sites_L,
                                                    trans_name = "Clearing_Of_Latency_From_Infected_Sites",
                                                    site_indices = site_indices,
                                                    trans_type = 5)


  ### combine transition rates ----

  # create empty list for transition rate storage
  trans_rates <- vector(mode = "list", length = 4)
  names(trans_rates) <- c("rate_type", "position", "rate", "source")

  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_infection, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_farm_recovery, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_fishery_latency, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_cleared, list_base = trans_rates)


  ## if contact tracing is enabled ----

  if(contact_tracing == TRUE) {

  ## calculate transition rates ----

  # Rate 5: rate at which contact traced sites will be tested
  rate_sites_ct_tested <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = sites_contact_traced,
                                                       trans_name = "Contact_Detection",
                                                       site_indices = site_indices,
                                                       trans_type = 12)

  ## combine transition rates ----

  trans_rates <- aquanet::combineTransitionRates(list_append = rate_sites_ct_tested, list_base = trans_rates)

  }


  ## if disease controls are applied -----

  if(disease_controls == TRUE) {

  ## calculate transition rates ----

  # Rate 6: rate at which fallow sites are disinfected
  rate_site_disinfected <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                        state_vector = sites_fallow,
                                                        trans_name = "Reinfection_After_Restocking_Const",
                                                        site_indices = site_indices,
                                                        trans_type = 1)

  # Rate 7: rate of detection in infected but undetected sites
  rate_site_detected <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                     state_vector = sites_I_undetected,
                                                     trans_name = "Detection_Reporting_Disease",
                                                     site_indices = site_indices,
                                                     trans_type = 6)

  # Rate 8: rate at which sites become fallow
  rate_site_fallow <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                   state_vector = sites_I_controlled,
                                                   trans_name = "Time_Required_Cull_Site",
                                                   site_indices = site_indices,
                                                   trans_type = 9)

  ## combine transition rates ----

  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_disinfected, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_detected, list_base = trans_rates)
  trans_rates <- aquanet::combineTransitionRates(list_append = rate_site_fallow, list_base = trans_rates)

  }


  ## if inside active transmission period get rates of transmission for mechanisms other than LFM: -----

  if (non_peak_season == FALSE) {

    ### calculate transition rates ----

    # Rate 9: rate at which sites revert from latent to clinical infection
    sites_L_recrudesce <- aquanet::listTransitionRates(run_time_params = run_time_params,
                                                       state_vector = sites_L,
                                                       trans_name = "Second_Outbreak_Due_To_Subclinical_Infection",
                                                       site_indices = site_indices,
                                                       trans_type = 4)

    # Rate 10: probability of a contact occurring downstream of an outbreak via the river network
    contacts_river <- aquanet::calcRiverTransmission(matrix_river_distances_prob = river_prob[["matrix_river_distances_prob"]],
                                                     clinical_state_vector = clinical_vector,
                                                     spread_restricted_off = spread_prevented_off,
                                                     spread_restricted_on = spread_prevented_on,
                                                     trans_type = 10)

    # Rate 11: probability of a contact occurring due to local fomite transmission
    contacts_fomite <- aquanet::calcRiverTransmission(matrix_river_distances_prob = site_distances_prob[["matrix_distances_probability"]],
                                                      clinical_state_vector = clinical_vector,
                                                      spread_restricted_off = spread_prevented_off,
                                                      spread_restricted_on = spread_prevented_on,
                                                      trans_type = 14)


    ### combine transition rates ----

    trans_rates <- aquanet::combineTransitionRates(list_append = sites_L_recrudesce, list_base = trans_rates)
    trans_rates <- aquanet::combineTransitionRates(list_append = contacts_river, list_base = trans_rates)
    trans_rates <- aquanet::combineTransitionRates(list_append = contacts_fomite, list_base = trans_rates)


    ## if there are susceptible sites without restrictions preventing spread on site: ----

    if (sum(!state_vector & !spread_prevented_on) != 0) {

      ### calculate & combine transition rates ----

      # Rate 12: identify transitions from infected to susceptible sites that could occur randomly regardless of mechanism
      # Note: excludes contacts from sites whose restrictions prevent this mechanism of transmission
      sites_random_change <- aquanet::calcRandomSpillover(clinical_state_vector = clinical_vector,
                                                          spread_restricted_off = spread_prevented_off,
                                                          spread_restricted_on = spread_prevented_on,
                                                          site_indices = site_indices,
                                                          trans_name = "Fomite_Transmission_Independant_Prob",
                                                          run_time_params = run_time_params,
                                                          trans_type = 11)

      trans_rates <- aquanet::combineTransitionRates(list_append = sites_random_change, list_base = trans_rates)

    }
  }

  return(list(trans_rates = trans_rates,
              catchment_movements = risk_contacts_catch_corrected[["catchment_movements"]],
              sites_all_movement_restricted = sites_all_movement_restricted))

}
