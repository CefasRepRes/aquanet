#' importAndCondense
#'
#' Loads and condenses multiple the full data (full_details) output from a scenario run. Data are
#' grouped by simulation number and siteID. The amount of time each site spends in each state is
#' summed at each state change in the simulation.
#'
#' @param scenario_name (class string) name of the scenario assigned in the `params.yaml`.
#'
#' @return (class data.table) data.table containing time spend consecutively in each of the
#' simulation states for each site and each simulation run. Multiple occurrences of a site in the
#' same state are counted separately. This contains the following information:
#' 1. `site_id` (integer) the site identification number.
#' 2. `modelID` (integer) site idenfication number generated in model
#' 3. `state` (integer) the state the site is in. TODO: add file with state definitions and link here
#' 4. `group` (integer)
#' 5. `sim_no` (integer) simulation number.
#' 5. `timeID` (integer) simulation time step ID.
#' 6. `t_diff` (numeric) sum of time spend in state.
#' 7. `t` (numeric) time at which change of state occurs.
#' 8. `trans_type` (numeric) transition type that results in change of state.
#' 9. onwards binary vectors indicating site type assigned in `site_types.csv`.
#' 25. `row_sums` (integer) the sum of the site type vector, and is used to diagnose uncategorised
#' sites (usually fisheries).
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

  # define output directory
  economics_dir <- here::here("outputs", scenario_name, "economics")

  # if economics directory doesn't exist, create it
  if (!dir.exists(economics_dir)) {
    dir.create(economics_dir)
    }

  # save as parquet file
  arrow::write_parquet(import_condense_all,
                       sink = here::here(economics_dir,
                                         paste0(scenario_name, "-details-condensed.parquet")))

  # return condensed files
  return(import_condense_all)

}
