#' doEvent
#'
#' This function implements a selected transition event (includes disease spread, control and
#' recovery events), updating both the infection status and control states of each site as a result
#' of the transition event and the amount of time each site or group of sites are in specific state
#' (see details).
#'
#' First, create variable a logical vector to store information on whether all sites in a catchment
#' have been in a post-fallow state for at least `days_before_catchment_restock` days and are
#' therefore ready to restock.
#'
#' Next, extract required information from function inputs: sites that are fallow,  sites subject to
#'  transition identified in `transition rates[["position"]]`, total number of possible site transitions, a
#' corrected probability of each transition occurring (number of transitions), and the number of
#' transition probabilities.
#'
#' Identify sites that are under surveillance with no visible infection and for these sites update
#' the `time_vector` by adding the time that has passed since the last step in the simulation
#' `tdiff`. This keeps record of the amount of time these sites are under surveillance.
#'
#' To initiate the event, if there is more than one transition probability select a transition event
#'  at random and for this event extract the site ID and rate type `trans_type` for the event.
#'
#' Depending on the `trans_type` of the selected transition event update the infection status of the
#'  site (whether it is listed as infected or susceptible in `state_vector`), update the controls in
#'  place at the site, and if transmission occurs from a known source record this information. If
#' the site transitions from Controlled -> Fallow, or Infected -> Latent, or Latent -> Susceptible
#' via recovery or clearance then reset the time in time_vector.
#'
#' Finally, update the time-dependent controls at the site within `control_matrix` for sites of
#' interest:
#'
#' For sites that are in stage 1 surveillance and have not been reinfected check whether sites have
#' been under surveillance for required number days and transition those that have to stage 2
#' surveillance.
#'
#' For sites that are in stage 2 surveillance and have not been reinfected check whether sites have
#' been under surveillance for required number of days and transition those that have to no disease
#' control state.
#'
#' If sites have been in a fallow state for required number of days convert controls to post-fallow
#' state. Check catchment level restocking to identify catchments containing only post-fallow sites
#'  and reset the `catchment_time_vector` to 0 for these. If these catchments have been in a ready
#' to restock state for greater than or equal to `days_before_catchment_restock` days then move the
#' sites within these catchments to a no disease control state and reset the `time_vector` for
#' these sites.
#'
#' @param state_vector (class numeric) numeric binary vector of length 'number of sites' containing
#' information about the state of each site in relation to a condition. This state vector should
#' specify whether a site is in an 1 = infected or 0 = susceptible state. (Note: created within the
#' `aquanet::simulationCode` function for loop).
#'
#' @param control_matrix (class matrix) matrix containing 7 columns depicting different control
#' states and rows depicting whether each sites is 1 = in the specified control state or 0 = not in
#' the specified control state.
#'
#' @param transition_rates (class list) of length 4 containing:
#' 1. (class numeric) vector of transition types.
#' 2. (class integer) vector of sites subject to transition.
#' 3. (class integer) vector of transition rates (transmission probability).
#' 4. (class numeric) vector of source sites (of disease in case of transmission).
#'
#' @param tdiff (class numeric) size of current time step generated in `aquanet::simulationCode()`
#' by picking a random number from exponential distribution weighted by sum of transition rates.
#'
#' @param move_restricted_sites (class logical) logical vector of length number of sites that
#' states whether movements at this site are currently restricted (TRUE) or unrestricted (FALSE).
#' (Note: created in the `aquanet::updateRates` function of aquanet-mod and also input to
#' `aquanet::excludeWithinCatchmentMovements` function).
#'
#' @param non_peak_season (class logical) logical indicating whether the current time step in the
#' model is within non peak season where disease transmission is lower.
#'
#' @param run_time_params (class data frame) of model run time parameters imported from original
#' parameter file which is subsequently split into model set up and model run time parameters. Data
#' frame contains probabilities from the scenarios of interest for listing transition rates.
#'
#' @param n_catchments (class integer) number of catchments.
#'
#' @param spmatrix_sites_catchment (class dgCMatrix, Matrix package) sparse matrix containing site
#' to catchment summary.
#'
#' @param time_vector (class numeric) vector of length 'number of sites' to record the amount of
#' simulation time that a site has remained in a susceptible/recovered or fallow state.
#'
#' @param catchment_time_vector (class numeric) vector of length `n_catchments` to record the
#' amount of simulation time since every site in a catchment has been ready to be restocked.
#'
#' @param catchments_with_post_fallow_only (class logical) vector of length number of catchments
#' (`n_catchments`) depicting whether catchments contain only post-fallow sites ready for restocking
#'  with no fallow sites.
#'
#' @param source_inf_vector (class numeric) vector of length 'number of sites' to track which sites
#' are responsible for infection when infection is transmitted via either Live Fish Movements or via
#' river network connectivity.
#'
#' @param source_inf_matrix (class matrix) matrix of dimensions 'number of sites' x 'number of
#' sites' to track the sites that are infected either by Live Fish Movements of via river network
#' connectivity'. Note: this matrix is used for forward contact tracing.
#'
#' @param contact_tracing (class logical) vector of length 1 indicating whether or not contact
#' tracing is taking place.
#'
#' @param days_before_catchment_restock (class numeric) number of days that all sites within a
#' catchment need to be in the post-fallow state before restocking.
#'
#'
#' @return (class list) of length 8 containing:
#' 1. (class numeric) `state_vector` numeric binary vector of length 'number of sites' containing
#' information about the state of each site in relation to a condition (E.g. is the site
#' 1 = infected or 0 = susceptible state). This state vector should specify whether a site is in an
#' 1 = infected or 0 = susceptible state. (Note: created within the `aquanet::simulationCode`
#' function for loop).
#' 2. (class matrix) `control_matrix` updated matrix containing 7 columns depicting different
#' control states and rows depicting whether each sites is 1 = in the specified control state or
#' 0 = not in the specified control state.
#' 3. (class numeric) `time_vector` updated vector of length 'number of sites' to record the amount
#' of simulation time that a site has remained in a susceptible/recovered or fallow state.
#' 4. (class numeric) `catchment_time_vector` updated vector of length `n_catchments` to record the
#' amount of simulation time since every site in a catchment has been ready to be restocked.
#' 5. (class logical) `catchments_with_post_fallow_only` vector of length (`n_catchments`)
#' depicting whether catchments contain only post-fallow sites ready for restocking with no fallow
#' sites.
#' 6. (class matrix) `source_inf_vector` updated matrix of dimensions 'number of sites' x 'number of
#'  sites' to track the sites that are infected either by Live Fish Movements of via river network
#' connectivity'. Note: this matrix is used for forward contact tracing.
#' 7. (class numeric) `rate_type` transition type for the site selected by `doEvent()` (extracted
#' from `transition_rates`).
#' 8. (class matrix) `source_inf_matrix` matrix of dimensions 'number of sites' x 'number of
#' sites' to track the sites that are infected either by Live Fish Movements of via river network
#' connectivity'. Note: this matrix is used for forward contact tracing.
#'
#' @export
#'
doEvent <- function(state_vector,
                    control_matrix,
                    transition_rates,
                    tdiff,
                    move_restricted_sites,
                    non_peak_season,
                    run_time_params,
                    n_catchments,
                    spmatrix_sites_catchment,
                    time_vector,
                    catchment_time_vector,
                    catchments_with_post_fallow_only,
                    source_inf_vector,
                    source_inf_matrix,
                    contact_tracing,
                    days_before_catchment_restock) {

  ## create variables to populate ----

  # create logical vector of catchments where every site has been ready to be restocked
  catchments_ready_restock <- rep(FALSE, n_catchments)


  ## extract transition information (sites, probabilities) ----

  # create logical vector of sites that are fallow
  sites_fallow <- as.logical(control_matrix[ , 4])

  # vector of sites subject to transition (possible infection events)
  site_vector <- transition_rates[["position"]]

  # calculate the total rates of transmission
  rates_total <- sum(transition_rates[["rate"]])

  # create vector of probabilities
  prob <- transition_rates[["rate"]] / rates_total
  n_prob <- length(prob)


  ## update model times ----

  # identify sites under surveillance with no visible infection
  # sites with movement restrictions (susceptible and not latent OR infected and latent) or fallow sites
  sites_surveillance <- (move_restricted_sites & !state_vector & !control_matrix[ , 6]) |
    (move_restricted_sites & state_vector & control_matrix[ , 6]) | sites_fallow

  # update time since application of controls (start counting when site has recovered)
  # time the period over which controls are applied, as well as the period a site has been fallow (mutually exclusive scenarios)
  time_vector[sites_surveillance] <- time_vector[sites_surveillance] + tdiff

  # increment the time recorded since every site in a catchment has been ready to be restocked
  catchment_time_vector[catchments_with_post_fallow_only] <- catchment_time_vector[catchments_with_post_fallow_only] + tdiff


  ## do the event ----

  # if there is more than one probability, pick an event number using the vector of probabilities
  if (n_prob != 1) {
    event <- sample.int(n_prob, size = 1, prob = prob, replace = TRUE)
  } else {
    event <- 1
  }

  # define site corresponding to event (Note: + 1 as 0-based site_vector)
  site <- site_vector[event] + 1

  # extract transition rate for selected event - and modify the state appropriately
  trans_type <- transition_rates[["rate_type"]][event]


  ## S --> I | L --> I ----
  # IF the transition rate is either via LFM, recrudescence from subclinical, river-based,
  # random mechanism-independent or fomite-based: assign the site an infectious state
  if (trans_type %in% c(0, 4, 10, 11, 14)) {
    state_vector[site] <- 1

    # IF it is non_peak_season: define the site as latently infected
    if (non_peak_season == TRUE) {
      control_matrix[site, 6] <- 1
      state_vector[site] <- 1
    }

    # ELSE define the site as having infection which can potentially be detected
    else {

      # IF the site has no controls in place: assign the site an infectious state
      if (sum(control_matrix[site, 2:5]) == 0) {
        control_matrix[site, 1] <- 1

        # IF the transition rate is via LFM or river-based:
        if (trans_type %in% c(0, 10)) {
          # extract infection source site and update source_inf_vector and source_inf_matrix
          # in case contact tracing applied later
          source_inf <- transition_rates[["source"]][event] + 1
          source_inf_vector[site] <- source_inf
          source_inf_matrix[source_inf,  site] <- 1
        }
      }

      # define the site as no longer latent
      control_matrix[site, 6] <- 0
    }
  }


  ## I --> L  (both farm and fishery) ----
  # IF the transition rate is either farm recovers or fishery becomes subclinical/latent:
  if (trans_type %in% c(2, 3)) {

    # IF the site has been infected but has recovered before controls implemented: assume it will not be controlled
    if (control_matrix[site, 1] == 1) {
      control_matrix[site, 1] <- 0

    # ELSE IF the site has been infected but has recovered and is under controls: reset clock
    } else if (sum(control_matrix[site, c(2, 3)]) == 1) {
      time_vector[site] <- 0
    }

    # define the site as latent
    control_matrix[site, 6] <- 1
  }


  ## I --> C transition | Traced Site --> C transition ----
  # IF the transition rate is either infection detected and reported or contact traced sites are tested:
  if (trans_type %in% c(6, 12)) {

    # place controls on sites that are infected and contact traced (i.e. contact traced sites tested)

    # IF the site is infected and contact traced:
    if (state_vector[site] == 1 && control_matrix[site, 7] == 1) {
      # define site as controlled and reset control ready to import and contact tracing
      # Note: site no longer needs to be contact traced as it has been tested through other means
      control_matrix[site, 2] <- 1
      control_matrix[site, c(1, 3, 7)] <- 0
    }

    # define the site as no longer contact traced (if not infected and contact traced)
    if(state_vector[site] == 0 && control_matrix[site, 7] == 1){
      control_matrix[site, 7] <- 0
    }


    # place controls on sites that are infected and not latent (i.e. infection detected and reported)

    # IF the site is infected and infection is not latent: site is controlled and zero ready to import
    # Note: clock is only reset when site is recovered and not when it is placed under control
    if (state_vector[site] == 1 && control_matrix[site, 6] == 0) {
      control_matrix[site, 2] <- 1
      control_matrix[site, c(1, 3)] <- 0
    }

    if(contact_tracing == TRUE) {
      # define source of infection
      source_inf <- source_inf_vector[site]

      # IF the source site of infection is known due to infection via LFM/river: reset to 0
      if (source_inf != 0) {
        source_inf_vector[site] <- 0

        # IF the source site of infection has no controls: update site for contact tracing
        # Note: don't test a site for infection if it has already been subject to controls
        if (sum(control_matrix[source_inf, 2:5]) == 0) {
          control_matrix[source_inf, 7] <- 1
        }
      }

    ## forward tracing (who was at risk before controls implemented at this site) ----
    # Note: any sites that have been in contact with the infected site and which transmitted infection via LFM or river
    infected_source <- source_inf_matrix[site, ]
    infected_sites <- which(infected_source == 1)

    # IF there are sites which may have been infected via contact with site: redefine as 0
    if(sum(infected_source) != 0){
      source_inf_matrix[site, ] <- 0

      # FOR each infected site: define it as the source
      for(i in 1:length(infected_sites)){
        source_site <- infected_sites[i]

        # IF the source site has no controls in place: define it as contact traced
        # Note: don't test a site for infection if it has already been subject to controls
        if(sum(control_matrix[source_site, 2:5]) == 0) {
          control_matrix[source_site,  7] <- 1
        }
      }
    }
  }
}

  ## C --> F transition ----
  # IF the transition rate is rate at which controlled sites become fallow:
  if (trans_type == 9) {
    # define the site as fallow and reset movement/stocking controls
    control_matrix[site, 4] <- 1
    control_matrix[site, c(2, 3)] <- 0

    # define the site as no longer latent
    control_matrix[site, 6] <- 0

    # reset the clock (to check how long the site has been fallow)
    # Note: a site cannot have multiple control states at the same time (so no interfere with other code)
    time_vector[site] <- 0

    # IF it is not the peak transmission season: define site as uninfected
    if (non_peak_season == TRUE) {
      state_vector[site] <- 0
    }
  }


  ## L -> S transition ----
  # IF the transition rate is rate at which site transitions from latent infection (farms and fisheries):
  if (trans_type == 5) {
    # redefine site as uninfected
    state_vector[site] <- 0

    # define the site as no longer latent
    control_matrix[site, 6] <- 0

    # reset the clock
    time_vector[site] <- 0
  }


  ## F,I --> F,S transition ----
  # IF the transition rate is rate at which fallow sites are disinfected: redefine site ad uninfected
  if (trans_type == 1) {
    state_vector[site] <- 0
  }


  ## update controls ----

  # Update controls 1: sites which have passed stage 1 surveillance without infection
  # extract minimum period fisheries are under stage 1 controls
  control_period <- run_time_params[["Early_Controls_Fisheries"]] # 'Se' in manuscript

  # logical vector of sites subject to movement controls but no longer infected
  sites_controlled_movements <- as.logical(control_matrix[ , 2] * !state_vector)

  # logical vector of sites which have passed stage 1 surveillance without infection
  sites_allow_moves_in <- (time_vector > control_period) & sites_controlled_movements

  # total number of sites which have passed stage 1 surveillance without infection
  n_sites_allow_moves_in <- sum(sites_allow_moves_in)

  # IF there are sites that have passed stage 1 surveillance without infection:
  if (n_sites_allow_moves_in != 0) {
    # remove movement restriction and allow restocking
    control_matrix[sites_allow_moves_in, 2] <- 0
    control_matrix[sites_allow_moves_in, 3] <- 1
  }


  # Update controls 2: sites which have passed stage 2 surveillance without infection
  # calculate minimum period fisheries are under stage 1 + 2 controls
  control_period <- run_time_params[["Early_Controls_Fisheries"]] + # 'Se' in manuscript
    run_time_params[["Late_Controls_Fisheries"]] # 'Sl' in manuscript

  # logical vector of sites subject to move controls but allowed to import fish
  sites_controlled_movements_imports <- as.logical(control_matrix[ , 3] * !state_vector)

  # logical vector of sites which have passed stage 2 surveillance without infection
  sites_allow_moves_all <- (time_vector > control_period) & sites_controlled_movements_imports

  # total number of sites with have passed stage 2 surveillance without infection
  n_sites_allow_moves_all <- sum(sites_allow_moves_all)

  # IF there are sites that have passed stage 2 surveillance without infection:
  if (n_sites_allow_moves_all != 0) {
    # remove final movement controls and reset clock
    control_matrix[sites_allow_moves_all, 3] <- 0
    time_vector[sites_allow_moves_all] <- 0
  }


  # Update controls 3: sites which have been fallow for more than X number of days
  # extract minimum period sites are fallow for
  control_period <- run_time_params[["Fallow_Period"]]

  # logical vector of sites which have been fallow for more than X number of days
  sites_recover <- (time_vector > control_period) & sites_fallow

  # total number of sites that have been fallow for more than X number of days
  n_sites_recover <- sum(sites_recover)

  # IF there are sites that have been fallow for more than X number of days
  if (n_sites_recover != 0) {
    # convert to post-fallow state
    control_matrix[sites_recover, 4] <- 0
    control_matrix[sites_recover, 5] <- 1

    # extract fallow and post-fallow site status at catchment level to inform restocking
    catchment_restocking <- aquanet::checkCatchmentLevelRestocking(control_matrix = control_matrix,
                                                                   spmatrix_sites_catchment = spmatrix_sites_catchment,
                                                                   n_catchments = n_catchments)

    # update catchment time vector for catchments containing fallow sites
    catchment_with_fallow_some <- catchment_restocking[["catchments_with_fallow_some"]]
    catchment_time_vector[catchment_with_fallow_some] <- 0

    # overwrite catchments_with_post_fallow_only to contain sites ready for restocking
    catchments_with_post_fallow_only <- catchment_restocking[["catchments_with_post_fallow_only"]]
  }

  # select catchments for restock - with only post-fallow sites ready to restock for >= X days
  catchments_ready_restock[catchments_with_post_fallow_only] <- catchment_time_vector[catchments_with_post_fallow_only] >= days_before_catchment_restock

  # total number of catchments ready for restock
  n_catchments_ready_restock <- sum(as.numeric(catchments_ready_restock))


  # IF there are catchments ready for restocking:
  if (n_catchments_ready_restock > 0) {
    # create logical vector of sites that are post-fallow state and in a catchment ready for restocking
    sites_restocked <- as.logical((spmatrix_sites_catchment * control_matrix[ , 5]) %*%
                                        catchments_ready_restock)

    # release sites from post-fallow and ready to restock status
    control_matrix[sites_restocked, 5] <- 0

    # reset the catchments ready for restock in catchments_with_post_fallow_only to FALSE
    catchments_with_post_fallow_only[catchments_ready_restock] <- FALSE

    # reset clocks for catchment time and site time post restocking
    catchment_time_vector[catchments_ready_restock] <- 0
    time_vector[sites_restocked] <- 0
  }

  return(list(state_vector = state_vector,
              control_matrix = control_matrix,
              time_vector = time_vector,
              catchment_time_vector = catchment_time_vector,
              catchments_with_post_fallow_only = catchments_with_post_fallow_only,
              source_inf_vector = source_inf_vector,
              trans_type = trans_type,
              source_inf_matrix = source_inf_matrix))
}
