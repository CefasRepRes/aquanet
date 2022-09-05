#' timePerStage
#'
#' Summarises the output of `loadResultsFullSiteType` so that each row is the
#' total amount of time (`t_total`) a site (`site_id`) spends in a state (`state`)
#' per simulation (`sim_no`). Also included are the site categories.
#'
#' Used for economic analyses
#'
#' @param scenario_name (class string) the name of the scenario being loaded. Assigned
#' in RunModelCommandLine.R
#'
#' @return (data.table) sims_site_time_summary
#' @export
#'
#'
timePerStage <- function(scenario_name){
  system.time({
    # Load dataset -----
    load(here::here("outputs",
                    scenario_name,
                    "economics",
                    paste0(scenario_name, "-economics.Rdata")))
    # Convert to data.table for speed
    sites_summary_type <- data.table(sites_summary_type)
    # Begin loop -----
    ## Loop over simulations =====
    # Create list of simulation numbers
    sims <- unique(sites_summary_type[, sim_no])
    # Empty data frame to save results into
    sims_site_time_summary <- data.table()
    for(k in 1:length(sims)){
      # Filter by simulation
      sim <- sites_summary_type[sim_no == sims[k]]
      ## Loop over sites =====
      # Create list of site IDs
      site_ID <- unique(sim[, site_id])
      # Empty data frame to store results
      store <- data.table()
      for(s in 1:length(site_ID)){
        # Filter by site
        by_site <- sim[site_id == site_ID[s]]
        # Create a record ID for each row
        by_site$recordID <- rownames(by_site)
        # Create a vector to store rows to be removed
        remove_row <- rep(NA, nrow(by_site))
        ## Loop over rows ======
        # This loop checks if he state matches that on the previous row.
        # If it does match, it sums the time (t_diff) and removes the previous row
        # If not, the time remains the same.
        for(i in 1:nrow(by_site)){
          # If the state is the same as the previous state, add the times together
          if((by_site$state[i] == data.table::shift(by_site$state)[i]) %in% TRUE){
            by_site$t_diff[i] <- by_site$t_diff[i] + by_site$t_diff[i - 1]
            # Save the previous row for removal
            remove_row[[i]] <- as.numeric(i - 1)
          } else { # If the states are not the same, save the time
            by_site$t_diff[i] <- by_site$t_diff[i]
          }
        }
        # Remove excess rows
        remove_row <- na.omit(remove_row)
        by_site <- by_site[!(recordID %in% remove_row)]
        # Bind to data frame
        store <- rbind(store, by_site)
      }
      # Bind to data frame
      sims_site_time_summary <- rbind(sims_site_time_summary, store)
      # Print the simulation number to see progress
      print(k)
    }
    # Remove t
    sims_site_time_summary <- sims_site_time_summary[, "t" := NULL]
    # Rename t_diff to something more appropriate
    colnames(sims_site_time_summary)[colnames(sims_site_time_summary) == "t_diff"] <- "t_total"
    ## Save the results =====
    save(sims_site_time_summary,
         file = here::here("outputs",
                           scenario_name,
                           "economics",
                           paste0(scenario_name, "-site-time-summary.Rdata")))
    return(sims_site_time_summary)
  })
}
