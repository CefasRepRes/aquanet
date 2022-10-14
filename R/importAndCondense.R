#' importAndCondense
#'
#' Loads and condenses the full data (full_details) outputted from a scenario run.
#'
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
#'
#' @export
#'
#' @import data.table
#' @importFrom here here
#' @importFrom arrow read_parquet write_parquet
#' @importFrom utils head
#'
importAndCondense <- function(scenario_name){

  # define column names used with data.table syntax
    # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  group <- timeID <- trans_type <- NULL

  # list files for import and condense
  filenames <- list.files(here::here("outputs",
                                     scenario_name,
                                     "full_results"),
                          pattern = "batchNo-*")

  # for each file name import and condense
  import_condense <- lapply(setNames(filenames, filenames), function(x) {

    # import the parquet file
    file <- arrow::read_parquet(here::here("outputs",
                                           scenario_name,
                                           "full_results",
                                           x))

    # convert to data.table for speed (and ensure tdiff is numeric)
    sims <- data.table::data.table(file)
    sims$tdiff <- as.numeric(sims$tdiff)
    sims$siteID <- as.numeric(sims$siteID)

    # create a record ID for each row (grouped by simNo and siteID)
    sims[ , recordID := seq_len(.N), by = .(simNo, siteID)]

    # add group columns to get lengths of consecutive states (grouped by simNo and siteID)
    sims[ , group := data.table::rleid(state), by = .(simNo, siteID)]

    # add tdiff for each consecutive time point where the state is the same
    sites_summary <- sims[ ,
                     .(timeID = min(timeID), tdiff = sum(tdiff), t = min(t), trans_type = utils::head(trans_type, 1)),
                     . (modelID, siteID, state, group, simNo)]

    # sanity check for state 3 - NOTE: this can be removed
    threes <- sites_summary[state %in% c(3, 13, 23, 33)]
    if(nrow(threes) == 0) return(warning("There are no state threes in your outputs. Check your simulation code"))

    # read in site type vector
    site_type <- data.table::fread(here::here("outputs",
                                            scenario_name,
                                            "site_types.csv"))
    # join to sites_summary
    sites_summary_type <- data.table::merge.data.table(sites_summary,
                                                       site_type,
                                                       all.x = TRUE,
                                                       by.x = "siteID",
                                                       by.y = "site_code")

    # rename columns
    data.table::setnames(sites_summary_type, old = "siteID", new = "site_id")
    data.table::setnames(sites_summary_type, old = "tdiff", new = "t_diff")
    data.table::setnames(sites_summary_type, old = "simNo", new = "sim_no")

    # return data table of summarised time in each state per site and simulation
    return(sites_summary_type)

  })

  # combine the lists of data.tables to get all condensed simulation data
  import_condense_all <- data.table::as.data.table(data.table::rbindlist(import_condense))

  # save as parquet file
  arrow::write_parquet(import_condense_all,
                       sink = here::here("outputs",
                                         scenario_name,
                                         "economics",
                                         paste0(scenario_name, "-details-condensed.parquet")))
  # return condensed files
  return(import_condense_all)

}
