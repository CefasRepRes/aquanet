#' loadResultsSummary
#'
#' Loads and wrangles the batch (summary) data outputted from a scenario run.
#' Returns a data frame with the following information for each timestep:
#' 1. `batch_no` the batch number
#' 2. `k` the run number within the batch
#' 3. `t` the model time
#' 4. `t_diff` the difference between the current time and the time at the previous timestep
#' (the duration of the previous timestep)
#' 5. `sim_no` the simulation number
#' 6. `rate_type` the transition type
#' 7. `no_controlled_catchments` the number of catchments under controls in that timestep
#' 8. `cumulative_no_infected_sites` the cumulative number of infected sites in the simulation
#'
#' @param scenario_name (class string) the name of the scenario being loaded. Assigned
#' in RunModelCommandLine.R
#'
#' @return (class data.frame) no_per_state_wide
#' @export
#'
#' @import data.table
#' @import here
#'
loadResultsSummary <- function(scenario_name){
  # Get the filepath
  location_summary <- paste0(here::here("outputs",
                                        scenario_name,
                                 "batch_results"),
                             "/")
  # List files within the filepath
  files <- base::list.files(path = location_summary,
                                 pattern = "\\.RData$",
                                 recursive = TRUE)
  # Get the batch numbers
  batch_no <- as.integer(substring(
    text = regmatches(x = files,
                      m = regexpr(perl = TRUE,
                                  text = files,
                                  pattern = "batchNo-[0-9]+")),
    first = 9))
  # Combine and order files and batch numbers
  list_files <- data.table::data.table(files = files,
                                       batch_no = batch_no)
  list_files <- list_files[base::order(batch_no),]

  # Create empty data table
    # 42 = 39 variables + 3 redundancy
    # 8 = number of assigned columns
  no_per_state_wide <- data.table::data.table(rep(0, 42 + 8))
  # Loop over each file and extract summary states table
  for (i in 1:nrow(list_files)) {
    base::load(file = paste0(location_summary,
                            base::as.character(list_files[i, c("files")])))
    no_per_state_wide[, as.character((ncol(no_per_state_wide) + 1) :
                                       (ncol(no_per_state_wide) +
                                          ncol(summaryStates.table)))
                      := summaryStates.table]
    print(i)
  }
  # Warning message if summaryStates.table doesn't match what is expected (50 rows)
  if(nrow(summaryStates.table) != (42 + 8)) {
    base::warning("The script may have been feed the wrong parameters.")
  }
  # Transpose no_per_state_wide
  no_per_state_wide <- data.frame(t(as.matrix(no_per_state_wide)))
  # Assign column names and state numbers
  colnames(no_per_state_wide) <- c("batch_no",
                                  "k",
                                  "t",
                                  "t_diff",
                                  "sim_no",
                                  "rate_type",
                                  "no_controlled_catchments",
                                  "cumulative_no_infected_sites",
                                  as.character(1:42))
  # Assign rownams
  rownames(no_per_state_wide) <- 1:nrow(no_per_state_wide)

  return(no_per_state_wide)
}
