#' commitResults
#'
#' Output information on sites that become infected or subject to control measures at each time step
#'  within each simulation of the model. Additionally, output information on size of each time step
#' within the model simulations. Results are saved as an .RData file within the Full_Details
#' directory.
#'
#' TODO: change Full_Details path to match Sarah's folder structure
#'
#' @param df_states (class data.table data.frame) data frame of zeros with dimensions number of rows
#'  = number of sites (`n_sites`) + number of states (`n_states`), and number of columns = commit
#'  interval (`commit_int`) + 1.
#'
#' @param df_time (class data.table data.frame) data frame of zeros with dimensions number of rows
#' = 2, and number of columns = commit interval (`commit_int`) + 1.
#'
#' @param n_states (class numeric) number of different combinations of states possible within the
#' model.
#'
#' @param n_sites (class numeric) number of sites within the contact network (model run).
#'
#' @param site_indices (class integer) vector of 0-based site indices of length number of sites
#' (`n_sites`).
#'
#' @param commit_int (class numeric) number of intervals at which results should be committed/saved.
#'
#' @param iteration_vector (class integer) integer vector of length `commit_int` containing values
#' 1:commit interval (`commit_int`).
#'
#' @param batch_num (class numeric) batch number for model run.
#'
#' @param simulation_num (class numeric) simulation number for model run.
#'
#' @param save_num (class numeric) number of full saves for model run.
#'
#' @param filepath_results (class string) path to results directory for model run.
#'
#' @return Saved .RData file containing two data frames located within the `filepath_results`
#' Full_Details results directory. Data frame 1: `sim_states` contains site ID and infection and
#' control status for every time step within a simulation.  Data frame 2: `sim_times` contains the
#' time step ID, simulation number and details of time step size.
#'
#' @export
#'
commitResults <- function(df_states,
                          df_time,
                          n_states,
                          n_sites,
                          site_indices,
                          commit_int,
                          iteration_vector,
                          batch_num,
                          simulation_num,
                          save_num,
                          filepath_results) {

  # create empty dgTMatrix to record site state at each step within the specified commit interval
  matrix_states <- as(object = as.matrix(df_states[((n_states + 1):(n_states + n_sites)), ]),
                      Class = "dgTMatrix")

  # create data frame of simulation site states at each iteration
  sim_states <- data.frame(siteID = as.integer(site_indices[(matrix_states@i + 1)] + 1),
                           state = as.integer(matrix_states@x),
                           timeID = as.integer(matrix_states@j + ((save_num - 1) * commit_int)),
                           simNo = as.integer(df_states[3, ])[matrix_states@j + 1])

  # create data frame of simulation times
  sim_times <- data.frame(timeID = as.integer(iteration_vector + ((save_num - 1) * commit_int)),
                          simNo = as.integer(df_states[3, ])[iteration_vector],
                          tdiff = as.numeric(df_time[1, ])[iteration_vector],
                          t = as.numeric(df_time[2, ])[iteration_vector])

  # save simulation site states and simulation times
  # TODO switch file path back to Sarah's new system post-testing
  save(sim_states,
       sim_times,
       file = paste(filepath_results,
                    "/FullDetails/batchNo-", batch_num,
                    "_simNo-", simulation_num,
                    "_NoCommits-", save_num,
                    ".RData",
                    sep=""),
       compress=FALSE)
}
