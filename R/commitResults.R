commitResults <- function(allStates.table,
                          allStates.table.t,
                          n_states,
                          contactp.length,
                          sites_indices,
                          commit_int,
                          batch_num,
                          simulation_num,
                          save_num,
                          locationSaveResults,
                          iterationID.vector) {

  # create empty dgTMatrix to record site state at each step within the specified commit interval
  allStates.matrix <- as(object = as.matrix(allStates.table[((n_states + 1):(n_states + contactp.length)), ]),
                         Class = "dgTMatrix")

  sim_states <- data.frame(siteID = as.integer(sites_indices[(allStates.matrix@i + 1)] + 1),
                           state = as.integer(allStates.matrix@x),
                           timeID = as.integer(allStates.matrix@j + ((save_num - 1) * commit_int)),
                           simNo = as.integer(allStates.table[3, ])[allStates.matrix@j + 1])

  sim_times <- data.frame(timeID = as.integer(iterationID.vector + ((save_num - 1) * commit_int)),
                          simNo = as.integer(allStates.table[3, ])[iterationID.vector],
                          tdiff = as.numeric(allStates.table.t[1, ])[iterationID.vector],
                          t = as.numeric(allStates.table.t[2, ])[iterationID.vector])

  save(sim_states,
       sim_times,
       file = paste(locationSaveResults,
                    "/FullDetails/batchNo-", batch_num,
                    "_simNo-", simulation_num,
                    "_NoCommits-", save_num,
                    ".RData",
                    sep = ""),
       compress = FALSE)
}
