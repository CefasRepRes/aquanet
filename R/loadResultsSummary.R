#' loadResultsSummary
#'
#' This function loads and manipulates the batch (summary) data outputted from a scenario run.
#'
#' @param scenario_name (class string) the name of the scenario being loaded. Assigned
#' in RunModelCommandLine.R
#'
#' @return (class data frame) containing the following information for each time step:
#' 1. `batch_no` the batch number
#' 2. `k` the run number within the batch
#' 3. `t` the model time
#' 4. `t_diff` the difference between the current time and the time at the previous time step
#' (the duration of the previous time step)
#' 5. `sim_no` the simulation number
#' 6. `rate_type` the transition type
#' 7. `no_controlled_catchments` the number of catchments under controls in that time step
#' 8. `cumulative_no_infected_sites` the cumulative number of infected sites in the simulation
#' Plus the number of sites in each state (1:39) plus redundancy (40:42)
#'
#' @export
#'
#' @importFrom data.table data.table
#' @importFrom here here
#'
loadResultsSummary <- function(scenario_name) {

  # define column names used with data.table syntax
    # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  output_summary_states <- NULL

  # get the file path
  location_summary <- paste0(here::here("outputs",
                                        scenario_name,
                                        "batch_results"), "/")

  # list .Rdata files within the file path
  files <- base::list.files(path = location_summary,
                            pattern = "\\.RData$",
                            recursive = TRUE)

  # get the batch numbers
  batch_no <- as.integer(substring(
    text = regmatches(x = files,
                      m = regexpr(perl = TRUE,
                                  text = files,
                                  pattern = "batchNo-[0-9]+")),
    first = 9))

  # Combine and order files and batch numbers
  list_files <- data.table::data.table(files = files,
                                       batch_no = batch_no)

  list_files <- list_files[base::order(batch_no), ]

  # create empty data table
  # 42 = 39 variables + 3 redundancy
  # 8 = number of assigned columns
  no_per_state_wide <- data.table::data.table(rep(0, 42 + 8))

  # loop over each file and extract summary states table
  for (i in 1:nrow(list_files)) {
    base::load(file = paste0(location_summary,
                             base::as.character(list_files[i, c("files")])))
    no_per_state_wide[ , as.character((ncol(no_per_state_wide) + 1) :
                                       (ncol(no_per_state_wide) +
                                          ncol(output_summary_states)))
                      := output_summary_states]
    print(i)
  }
  # warning message if output_summary_states doesn't match what is expected (50 rows)
  if(nrow(output_summary_states) != (42 + 8)) {
    base::warning("The script may have been feed the wrong parameters.")
  }

  # Transpose no_per_state_wide
  no_per_state_wide <- data.frame(t(as.matrix(no_per_state_wide)))

  # assign column names and state numbers
  colnames(no_per_state_wide) <- c("batch_no",
                                   "k",
                                   "t",
                                   "t_diff",
                                   "sim_no",
                                   "rate_type",
                                   "no_controlled_catchments",
                                   "cumulative_no_infected_sites",
                                   as.character(1:42))

  # assign rownames
  rownames(no_per_state_wide) <- 1:nrow(no_per_state_wide)
  return(no_per_state_wide)
}
