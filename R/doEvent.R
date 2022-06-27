do_event <- function(state_vector,
                     control_matrix,
                     transition.rates,
                     tdiff,
                     movement.restrictions.bySite,
                     catchment_time_vector,
                     catchments.all.sites.c5.status,
                     source.infection.vector,
                     infected.source.matrix,
                     non_peak_season,
                     run_time_params,
                     n_catchments,
                     spmatrix_sites_catchment) {

  controlled.sites.c4.logical <- as.logical(control_matrix[, 4])

  # Update vector showing time since application of controls
  # Only start counting when the site has recovered
  # Time the period over which controls are applied, as well as the period a site has been fallow (these cases are mutally exclusive)
  surveillance.noVisibleInfection <- (movement.restrictions.bySite & !state_vector & !control_matrix[,6]) | (movement.restrictions.bySite & state_vector & control_matrix[,6]) | controlled.sites.c4.logical
  time_vector[surveillance.noVisibleInfection] <- time_vector[surveillance.noVisibleInfection] + tdiff

  # Increment the time recorded since every site in a catchment has been ready to be restocked
  catchment_time_vector[catchments.all.sites.c5.status] <- catchment_time_vector[catchments.all.sites.c5.status] + tdiff

  # Vector of possible events, expressed as the site to be infected if an event occurred
  site.vector <- transition.rates[[2]]

  # Calculate the total rate
  rate.total <- sum(transition.rates[[3]])

  # Create a vector of probabilities
  p <- transition.rates[[3]] / rate.total
  no.rates <- length(p)

  # Pick an event number, from those available, using the vector of probabilities
  if (no.rates != 1) {
    event <- sample.int(no.rates, size=1, prob = p, replace = TRUE)
  } else {
    event <- 1
  }

  # Check the site corresponding to a particular event
  site <- site.vector[event] + 1

  # Lookup the event number and type, and modify the state appropriately
  rate.type <- transition.rates[[1]][event]

  ##### S --> I | L --> I  ####
  if (rate.type %in% c(0, 4, 10, 11, 14)) {

    # Note the site is in an infectious state
    state_vector[site] <- 1

    # If it is non_peak_season, identify the site as latent
    if (non_peak_season == TRUE) {
      control_matrix[site, 6] <- 1
      state_vector[site] <- 1
    }

    # Identify the site as having infection which can potentially be detected
    else {

      # Lookup the source of the infection and record it, in case contact tracing
      # needs to be applied at a later point
      if (sum(control_matrix[site, 2:5]) == 0) {
        control_matrix[site, 1] <- 1

        if (rate.type %in% c(0,10)) {
          source.infection <- transition.rates[[4]][event] + 1
          source.infection.vector[site] <- source.infection
          ########
          infected.source.matrix[source.infection,  site] <- 1
        }
      }

      # Note the site is no longer latent
      control_matrix[site, 6] <- 0
    }
  }

  # I --> L - for both farm and fishery
  if (rate.type %in% c(2,3)) {
    # If the site has been infected, but has recovered, before being placed under control, assume that it will not be controlled
    # If the site is controlled, and has recovered, reset the clock
    if (control_matrix[site, 1] == 1) {
      control_matrix[site, 1] <- 0
    } else if (sum(control_matrix[site, c(2,3)]) == 1) {
      time_vector[site] <- 0
    }

    control_matrix[site, 6] <- 1
  }

  # I --> C transition | Traced Site --> C transition
  if (rate.type %in% c(6, 12)) {
    # Note the site no longer needs to be contact traced,
    # since it has been tested through other means
    if (state_vector[site] == 1 && control_matrix[site, 7] == 1) {
      control_matrix[site, 2] <- 1
      control_matrix[site, c(1,3,7)] <- 0
    }

    control_matrix[site, 7] <- 0


    # Note the site is now in a controlled state, provided it is still infected
    # Clock is only reset when site is recovered, not when it is placed under control
    if (state_vector[site] == 1 && control_matrix[site, 6] == 0) {
      control_matrix[site, 2] <- 1
      control_matrix[site, c(1,3)] <- 0
    }

    # Note any sites which have been in contact with
    # the infected site and which transmitted infection
    # via live fish movements or river-based transmission
    source.infection <- source.infection.vector[site]

    if (source.infection != 0) {
      source.infection.vector[site] <- 0

      # Don't test a site for infection if it has already
      # been subject to controls
      if (sum(control_matrix[source.infection, 2:5]) == 0) {
        control_matrix[source.infection, 7] <- 1
      }
    }

    ######## Forward Tracing
    # Note any sites which have been in contacted by
    # the infected site and which transmitted infection
    # via live fish movements or river-based transmission
    infected.source <- infected.source.matrix[site,]
    infected.sites <- which(infected.source == 1)


    if(sum(infected.source) != 0){
      infected.source.matrix[site,] <- 0

      for(i in 1:length(infected.sites)){
        # Don't test a site for infection if it has already
        # been subject to controls
        source.site <- infected.sites[i]
        if(sum(control_matrix[source.site, 2:5]) == 0) {
          control_matrix[source.site,  7] <- 1
        }
      }
    }

    ########
  }

  # C --> F transition
  if (rate.type == 9) {
    # Note the site is now fallow
    control_matrix[site, 4] <- 1
    control_matrix[site, c(2,3)] <- 0

    # Note the site is no longer latent
    control_matrix[site, 6] <- 0

    # Reset the time on the clock, so that it is possible to check how long the site has been fallow
    # A site can not have multiple control states at the same time, so this should not interfere with other code
    time_vector[site] <- 0

    if (non_peak_season == TRUE) {
      state_vector[site] <- 0
    }
  }

  # L -> S transition
  if (rate.type == 5) {
    # Note S state
    state_vector[site] <- 0

    # Note the site is no longer latent
    control_matrix[site, 6] <- 0

    time_vector[site] <- 0

  }

  # F,I --> F,S transition (decontamination of fallow sites)
  if (rate.type == 1) {
    # Note S state
    state_vector[site] <- 0

  }

  # Update controls on those sites which have passed a given no. days without infection
  ## min.trans <- run_time_params[[7]]
  min.trans <- 720
  controlled.sites.c2.logical <- as.logical(control_matrix[, 2]*!state_vector)
  allow.inward.movements <- (time_vector > min.trans) & controlled.sites.c2.logical
  allow.inward.movements.no <- sum(allow.inward.movements)

  if (allow.inward.movements.no != 0) {
    control_matrix[allow.inward.movements, 2] <- 0
    control_matrix[allow.inward.movements, 3] <- 1
  }

  # Update controls on those sites which have passed a given no. days without infection
  min.trans <- 360 + run_time_params[[8]]
  controlled.sites.c3.logical <- as.logical(control_matrix[, 3])
  allow.all.movements <- (time_vector > min.trans) & controlled.sites.c3.logical
  allow.all.movements.no <- sum(allow.all.movements)

  if (allow.all.movements.no != 0) {
    control_matrix[allow.all.movements, 3] <- 0
    time_vector[allow.all.movements] <- 0
  }

  # Update controls on those sites which have been fallow for more than x number of days
  min.trans <- run_time_params[[10]]
  recover.site <- (time_vector > min.trans) & controlled.sites.c4.logical
  recover.site.no <- sum(recover.site)

  if (recover.site.no != 0) {
    control_matrix[recover.site, 4] <- 0
    control_matrix[recover.site, 5] <- 1

    checkCatchmentLevelRestocking.objects <- checkCatchmentLevelRestocking(control_matrix, tdiff)
    control_matrix <- checkCatchmentLevelRestocking.objects[[1]]
    catchments.some.sites.c4.status <- checkCatchmentLevelRestocking.objects[[2]]
    catchments.all.sites.c5.status <- checkCatchmentLevelRestocking.objects[[3]]
    catchment_time_vector[catchments.some.sites.c4.status] <- 0
  }

  # Identify catchments where every site has been ready to be restocked, for more than four days
  catchments.ready.restock <- rep(FALSE, n_catchments)
  catchments.ready.restock[catchments.all.sites.c5.status] <- catchment_time_vector[catchments.all.sites.c5.status] >= 4
  no.catchments.ready.restock <- sum(as.numeric(catchments.ready.restock))

  # Print information on catchments where every site has been ready to be restocked, for more than four days
  if (no.catchments.ready.restock > 0) {
    sitesReadyRestocked <- as.logical((spmatrix_sites_catchment * control_matrix[,5]) %*% catchments.ready.restock)

    control_matrix[sitesReadyRestocked, 5] <- 0
    ###
    #control_matrix[sitesReadyRestocked, 3] <- 1
    ###

    catchments.all.sites.c5.status[catchments.ready.restock] <- FALSE
    catchment_time_vector[catchments.ready.restock] <- 0

    time_vector[sitesReadyRestocked] <- 0
  }

  return(list(state_vector, control_matrix, time_vector, catchment_time_vector, catchments.all.sites.c5.status, source.infection.vector, rate.type, infected.source.matrix))
}