do_event <- function(state_vector,
                     control_matrix,
                     transition_rates,
                     tdiff,
                     move_restricted_sites,
                     non_peak_season,
                     run_time_params,
                     n_catchments,
                     n_sites,
                     spmatrix_sites_catchment) {

  ## create variables to populate ----

  # create vector to record time since catchment status changed
  catchment_time_vector <- rep(0, length = n_catchments)

  # create vector to record catchments in post-fallow state
  catchments_with_post_fallow_only <- rep(0, length = n_catchments)

  # create vector to track source sites of infection (via fish movements / river network)
  source_inf_vector <- rep(0, n_sites)

  # create matrix to track sites infected (via fish movements / river network) to forward contact trace
  source_inf_matrix <- matrix(data = 0, nrow = n_sites, ncol = n_sites)

  # create logical vector of sites that are fallow
  sites_fallow <- as.logical(control_matrix[, 4])


  ## extract transition information (sites, probabilities) ----

  # vector of sites subject to transition (possible infection events)
  site_vector <- transition_rates[[2]]

  # calculate the total rates of transmission
  rates_total <- sum(transition_rates[[3]])

  # create vector of probabilities
  prob <- transition_rates[[3]] / rates_total
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
  rate_type <- transition_rates[[1]][event]


  ## S --> I | L --> I ----
  # IF the transition rate is either via LFM, recrudescence from subclinical, river-based,
  # random mechanism-independent or fomite-based: assign the site an infectious state
  if (rate_type %in% c(0, 4, 10, 11, 14)) {
    state_vector[site] <- 1

    # IF it is non_peak_season: define the site as latently infected
    if (non_peak_season == TRUE) {
      control_matrix[site, 6] <- 1
      state_vector[site] <- 1
    }

    # ELSE define the site as having infection which can potentially be detected
    else {

      # Lookup the source of the infection and record it, in case contact tracing
      # needs to be applied at a later point

      # IF the site has no controls in place: assign the site an infectious state
      if (sum(control_matrix[site, 2:5]) == 0) {
        control_matrix[site, 1] <- 1

        # IF the transition rate is via LFM or river-based:
        if (rate_type %in% c(0, 10)) {
          # extract infection source site and update source_inf_vector and source_inf_matrix
          source_inf <- transition_rates[[4]][event] + 1
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
  if (rate_type %in% c(2, 3)) {

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
  if (rate_type %in% c(6, 12)) {
    # IF the site is infected and in an infected catchment:
    if (state_vector[site] == 1 && control_matrix[site, 7] == 1) {
      # define sute as controlled and zero ready to import and catchment controls/contact tracing
      # Note: site no longer needs to be contact traced as it has been tested through other means
      control_matrix[site, 2] <- 1
      control_matrix[site, c(1, 3, 7)] <- 0
    }

    # define the site as no longer latent
    control_matrix[site, 7] <- 0


    # IF the site is infected and infection is not latent: site is controlled and zero ready to import
    # Note: clock is only reset when site is recovered and not when it is placed under control
    if (state_vector[site] == 1 && control_matrix[site, 6] == 0) {
      control_matrix[site, 2] <- 1
      control_matrix[site, c(1, 3)] <- 0
    }

    # define source of infection
    source_inf <- source_inf_vector[site]

    # IF the source site of infection is not 0: redefine as 0
    if (source_inf != 0) {
      source_inf_vector[site] <- 0

      # IF the source site of infection has no controls: place under catchment controls/contact tracing
      # Note: don't test a site for infection if it has already been subject to controls
      if (sum(control_matrix[source_inf, 2:5]) == 0) {
        control_matrix[source_inf, 7] <- 1
      }
    }

    ## forward tracing ----
    # Note: any sites that have been in contact with the infected site and which transmitted infection via LFM or river
    infected_source <- source_inf_matrix[site, ]
    infected_sites <- which(infected_source == 1)

    # IF there are infected sites in contact with site: redefine as 0
    if(sum(infected_source) != 0){
      source_inf_matrix[site, ] <- 0

      # FOR each infected site: define it as a source
      for(i in 1:length(infected_sites)){
        source_site <- infected_sites[i]

        # IF the site has no controls in place: place under catchment controls/contact tracing
        # Note: don't test a site for infection if it has already been subject to controls
        if(sum(control_matrix[source_site, 2:5]) == 0) {
          control_matrix[source_site,  7] <- 1
        }
      }
    }
  }


  ## C --> F transition ----
  # IF the transition rate is rate at which controlled sites become fallow:
  if (rate_type == 9) {
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
  if (rate_type == 5) {
    # redefine site as uninfected
    state_vector[site] <- 0

    # define the site as no longer latent
    control_matrix[site, 6] <- 0

    # reset the clock
    time_vector[site] <- 0
  }


  ## F,I --> F,S transition ----
  # IF the transition rate is rate at which fallow sites are disinfected: redefine site ad uninfected
  if (rate_type == 1) {
    state_vector[site] <- 0
  }


  ## update controls ----

  # update controls 1: sites which have passed a given number of days without infection
  # extract minimum period fisheries are under stage 1 controls
  control_period <- run_time_params[["Early_Controls_Fisheries"]] # 'Se' in manuscript

  # logical vector of sites subject to movement controls but no longer infected
  sites_controlled_movements <- as.logical(control_matrix[ , 2] * !state_vector)

  # logical vector of sites which have passed a given number of days without infection
  sites_allow_moves_in <- (time_vector > control_period) & sites_controlled_movements

  # total number of sites which have passed a given number of days without infection
  allow.inward.movements.no <- sum(sites_allow_moves_in)

  # IF there are sites that have passed a given number of days without infection:
  if (allow.inward.movements.no != 0) {
    # remove movement restriction and allow restocking
    control_matrix[sites_allow_moves_in, 2] <- 0
    control_matrix[sites_allow_moves_in, 3] <- 1
  }


  # update controls 2: sites which have passed a given number days without infection
  # calculate minimum period fisheries are under stage 1 + 2 controls
  control_period <- run_time_params[["Early_Controls_Fisheries"]] + # 'Se' in manuscript
    run_time_params[["Late_Controls_Fisheries"]] # 'Sl' in manuscript

  controlled.sites.c3.logical <- as.logical(control_matrix[ , 3])
  allow.all.movements <- (time_vector > control_period) & controlled.sites.c3.logical
  allow.all.movements.no <- sum(allow.all.movements)

  if (allow.all.movements.no != 0) {
    control_matrix[allow.all.movements, 3] <- 0
    time_vector[allow.all.movements] <- 0
  }


  # update controls 3: sites which have been fallow for more than x number of days
  control_period <- run_time_params[["Fallow_Period"]]

  recover.site <- (time_vector > control_period) & sites_fallow
  recover.site.no <- sum(recover.site)


  if (recover.site.no != 0) {
    control_matrix[recover.site, 4] <- 0
    control_matrix[recover.site, 5] <- 1

    checkCatchmentLevelRestocking.objects <- checkCatchmentLevelRestocking(control_matrix, tdiff)

    control_matrix <- checkCatchmentLevelRestocking.objects[[1]]

    catchments.some.sites.c4.status <- checkCatchmentLevelRestocking.objects[[2]]
    catchments_with_post_fallow_only <- checkCatchmentLevelRestocking.objects[[3]]
    catchment_time_vector[catchments.some.sites.c4.status] <- 0
  }

  # Identify catchments where every site has been ready to be restocked, for more than four days
  catchments.ready.restock <- rep(FALSE, n_catchments)

  catchments.ready.restock[catchments_with_post_fallow_only] <- catchment_time_vector[catchments_with_post_fallow_only] >= 4
  no.catchments.ready.restock <- sum(as.numeric(catchments.ready.restock))

  # Print information on catchments where every site has been ready to be restocked, for more than four days
  if (no.catchments.ready.restock > 0) {
    sitesReadyRestocked <- as.logical((spmatrix_sites_catchment * control_matrix[ , 5]) %*% catchments.ready.restock)

    control_matrix[sitesReadyRestocked, 5] <- 0
    ###
    #control_matrix[sitesReadyRestocked, 3] <- 1
    ###

    catchments_with_post_fallow_only[catchments.ready.restock] <- FALSE
    catchment_time_vector[catchments.ready.restock] <- 0

    time_vector[sitesReadyRestocked] <- 0
  }

  return(list(state_vector,
              control_matrix,
              time_vector,
              catchment_time_vector,
              catchments_with_post_fallow_only,
              source_inf_vector,
              rate_type,
              source_inf_matrix))
}
