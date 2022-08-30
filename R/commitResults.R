#' commitResults
#'
#' Output information on sites that become infected or subject to control measures at each time step
#'  within each simulation of the model. Additionally, output information on size of each time step
#' within the model simulations. Results are saved as an .RData file within the full_results
#' directory.
#'
#' @param df_states (class data.table data.frame) data frame of zeros with dimensions number of rows
#'  = number of sites (`n_sites`) + number of states (`n_states`), and number of columns = commit
#'  interval (`commit_int`) + 1.
#'
#' @param df_site_names (class data.frame) data frame with columns siteID and modelID where number
#' of rows = number of sites (`n_sites`).
#'
#' @param n_states (class numeric) number of different combinations of states possible within the
#' model (note, include some redundancy here).
#'
#' @param n_sites (class numeric) number of sites within the contact network.
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
#' @return Saved .RData file containing data frame located within the `filepath_results`
#' full_results directory. Output data frame `sims` contains model ID, site ID, infection and
#' control status for every time step within a simulation, time step ID, simulation number and
#' details of time step size.
#'
#' @export
#'
commitResults <- function(df_states,
                          df_site_names,
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
  matrix_states <- as(object = as.matrix(df_states[((3 + n_states + 1):(3 + n_states + n_sites)), ]),
                      Class = "dgTMatrix")

  # create data frame of simulation site states and times at each iteration
  sims <- data.frame(modelID = as.integer(site_indices[(matrix_states@i + 1)] + 1),
                     state = as.integer(matrix_states@x),
                     timeID = as.integer(matrix_states@j + ((save_num - 1) * commit_int)),
                     simNo = as.integer(df_states[3, ])[matrix_states@j + 1],
                     tdiff = as.numeric(df_states[4, ])[matrix_states@j + 1],
                     t = as.numeric(df_states[5, ])[matrix_states@j + 1])

  # merge simulation data with true siteID and order by timeID
  sims <- merge(df_site_names, sims, by = "modelID", all.y = TRUE)
  sims <- sims[order(sims$timeID), ]
  rownames(sims) <- NULL # reset row names in case downstream elements rely on this

  # save simulation site states and simulation times
  save(sims,
       file = paste(filepath_results,
                    "/full_results/batchNo-", batch_num,
                    "_simNo-", simulation_num,
                    "_NoCommits-", save_num,
                    ".RData",
                    sep=""),
       compress=FALSE)
}
