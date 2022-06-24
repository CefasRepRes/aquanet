commitResults <- function(allStates.table,
                          allStates.table.t,
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
  allStates.matrix <- as(object = as.matrix(allStates.table[((n_states + 1):(n_states + n_sites)), ]),
                         Class = "dgTMatrix")

  # create data frame of simulation site states at each iteration
  sim_states <- data.frame(siteID = as.integer(site_indices[(allStates.matrix@i + 1)] + 1),
                           state = as.integer(allStates.matrix@x),
                           timeID = as.integer(allStates.matrix@j + ((save_num - 1) * commit_int)),
                           simNo = as.integer(allStates.table[3, ])[allStates.matrix@j + 1])

  # create data frame of simulation times
  sim_times <- data.frame(timeID = as.integer(iteration_vector + ((save_num - 1) * commit_int)),
                          simNo = as.integer(allStates.table[3, ])[iteration_vector],
                          tdiff = as.numeric(allStates.table.t[1, ])[iteration_vector],
                          t = as.numeric(allStates.table.t[2, ])[iteration_vector])

  # save simulation site states and simulation times
  filepath_save <- paste(filepath_results,
                         "/batch_results/states-batchNo-", batch_num,
                         "_simNo-", simulation_num,
                         "_NoCommits-", save_num,
                         sep = "")

  write.csv(x = sim_states,
            file = paste(filepath_save, "_simStates.csv"))
  write.csv(x = sim_times,
            file = paste(filepath_save, "_simTimes.csv"))
}
