commitResults <- function(df_states,
                          df_time,
                          n_states,
                          n_sites,
                          site_indices,
                          commit_int,
                          iteration_vector,
                          filepath_results,
                          batch_num,
                          simulation_num,
                          save_num) {

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
