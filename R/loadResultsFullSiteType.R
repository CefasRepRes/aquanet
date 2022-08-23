#' loadResultsFullSiteType
#'
#' Loads and wrangles the full data (full_details) outputted from a scenario run.
#' Returns a data frame with the following information for each timestep:
#' 1. `site_id` (numeric) the site identification number
#' 2. `state` (numeric) the state the site is in.
#' TODO: add file with state definitions and link here
#' 2. `sim_no` (numeric) the simulation number
#' 3. `t_diff` (numeric) the amount of time the site has spent in the state specified (in days)
#' 4. `t` (numeric) the model time
#'
#' Columns 5:20 are logical site type vectors assigned in `site_types.csv`-
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
#' in RunModelCommandLine.R
#'
#' @return (class data.table) sites_summary_type
#' @export
#'
#' @import data.table
#' @import here
#'
loadResultsFullSiteType <- function(scenario_name){
  # Create economics folder
  dir.create(file.path(here::here("outputs",
                                  scenario_name,
                                  "economics")),
             showWarnings = FALSE)
  # Get filenames of where outputs are
  filenames <- list.files(here::here("outputs",
                                     scenario_name,
                                     "full_results"),
                          pattern = "batchNo-*")
  # Load simulation outputs
  sims_all <- data.frame()
  for(i in 1:length(filenames)){
    load(here::here("outputs",
                    scenario_name,
                    "full_results",
                    filenames[i]))
    sims_all <- rbind(sims_all, sims)}
  # Select relevant columns
  sites_summary <- data.table(sims_all)
  sites_summary <- sites_summary[, .(siteID,
                                     state,
                                     simNo,
                                     tdiff,
                                     t)]
  threes <- sites_summary[state %in% c(3, 13, 23, 33)]
  if(nrow(threes) == 0) return(warning("There are no state threes in your outputs. Check your simulation code"))
  # Change siteID to modelID (because that's what it is)
  colnames(sites_summary)[1] <- "modelID"
  # Get actual site id
  site_id_mod <- data.table::fread(here::here("outputs",
                                              scenario_name,
                                              "site_details_with_model_id.csv"))
  site_id_mod <- site_id_mod[, .(siteID,
                                 modelID)]
  sites_summary <- base::merge(sites_summary,
                         site_id_mod,
                         all.x = TRUE,
                         by = "modelID")
  sites_summary <- sites_summary[, modelID := NULL]
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
  setnames(sites_summary_type,
           old = "siteID",
           new = "site_id")
  setnames(sites_summary_type,
           old = "tdiff",
           new = "t_diff")
  setnames(sites_summary_type,
           old = "simNo",
           new = "sim_no")
  # Save as R datafile
  save(sites_summary_type,
       file = here::here("outputs",
                         scenario_name,
                         "economics",
                         paste0(scenario_name, "-economics.Rdata")))
  return(sites_summary_type)
}
