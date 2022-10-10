#' importAndCondense
#'
#' Loads and condenses the full data (full_details) outputted from a scenario run.
#' Returns a data frame with the following information for each site in each simulation:
#' 1. `site_id` (numeric) the site identification number
#' 2. `state` (numeric) the state the site is in.
#' TODO: add file with state definitions and link here
#' 2. `sim_no` (numeric) the simulation number
#' 3. `t_total` (numeric) the total amount of time the site has spent in the state specified (in days)
#' 4. `trans_type` (numeric) the transition type which is occuring at that timestep
#'
#' Columns 6:21 are logical site type vectors assigned in `site_types.csv`-
#' whether or not a site contains that operation type.
#'
#' e.g. smallhatch = 1 means the site contains a small hatchery.
#'
#' e.g. farm_vector = 0 means the site is NOT a farm.
#'
#' Column 21 is the sum of the site type vector, and is used to diagnosed uncategorised
#' sites (usually fisheries)
#'
#' @param scenario_name (class string) the name of the scenario being loaded. Assigned
#' in the params.yaml
#'
#' @return (class data.table) sites_summary_type
#' @export
#'
#' @import data.table
#' @import here
#' @importFrom arrow read_parquet
#'
importAndCondense <- function(scenario_name){
  filenames <- list.files(here::here("outputs",
                                     scenario_name,
                                     "full_results"),
                          pattern = "batchNo-*")
  sims_all <- data.frame()
  for(j in 1:length(filenames)){
    arrow::read_parquet(here::here("outputs",
                    scenario_name,
                    "full_results",
                    filenames[j]))
    # Convert to data.table for speed
    sims <- data.table::data.table(sims)
    # Begin loop -----
    ## Loop over simulations =====
    # Create list of simulation numbers
    sim_nos <- unique(sims[, simNo])
    sims_time_summary <- data.frame()
    for(k in 1:length(sim_nos)){
      # Filter by simulation
      sim <- sims[simNo == sim_nos[k]]
      ## Loop over sites =====
      # Create list of site IDs
      site_ID <- unique(sim[, siteID])
      # Empty data frame to store results
      store <- data.frame()
      for(s in 1:length(site_ID)){
        # Filter by site
        by_site <- sim[siteID == site_ID[s]]
        # Create a record ID for each row
        by_site$recordID <- rownames(by_site)
        # Create a vector to store rows to be removed
        remove_row <- rep(NA, nrow(by_site))
        ## Loop over rows ======
        # This loop checks if he state matches that on the previous row.
        # If it does match, it sums the time (t_diff) and removes the previous row
        # If not, the time remains the same.
        if(nrow(by_site) > 1){
          for(i in 2:nrow(by_site)){
            # If the state is the same as the previous state, add the times together
            if((by_site$state[i] == by_site$state[i - 1]) %in% TRUE){
              by_site$tdiff[i] <- by_site$tdiff[i] + by_site$tdiff[i - 1]
              # Save the previous row for removal
              remove_row[[i]] <- as.numeric(i - 1)
            } else { # If the states are not the same, save the time
              by_site$tdiff[i] <- by_site$tdiff[i]
            }}
        }
        # Remove excess rows
        remove_row <- na.omit(remove_row)
        by_site <- by_site[!(recordID %in% remove_row)]
        # Bind to data frame
        store <- rbind(store, by_site)
      }
      # Bind to data frame
      sims_all <- rbind(sims_all, store)
      print(sim_nos[k])
    }# Print the file name to see progress
    print(filenames[j])
  }
  # Format and check for threes
  sites_summary <- data.table::data.table(sims_all)
  sites_summary$siteID <- as.numeric(sites_summary$siteID)
  threes <- sites_summary[state %in% c(3, 13, 23, 33)]
  if(nrow(threes) == 0) return(warning("There are no state threes in your outputs. Check your simulation code"))
  # Read in site type vector
  site_type <- data.table::fread(here::here("outputs",
                                            scenario_name,
                                            "site_types.csv"))
  # Join to summary
  sites_summary_type <- base::merge(sites_summary,
                                    site_type,
                                    all.x = TRUE,
                                    by.x = "siteID",
                                    by.y = "site_code")
  # Rename columns
  data.table::setnames(sites_summary_type,
                       old = "siteID",
                       new = "site_id")
  data.table::setnames(sites_summary_type,
                       old = "tdiff",
                       new = "t_diff")
  data.table::setnames(sites_summary_type,
                       old = "simNo",
                       new = "sim_no")
  # Save as R datafile
  save(sites_summary_type,
       file = here::here("outputs",
                         scenario_name,
                         "economics",
                         paste0(scenario_name, "-details-condensed.Rdata")))
}
