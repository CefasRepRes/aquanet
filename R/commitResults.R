commitResults <- function(allStates.table,
                          allStates.table.t,
                          numberFullSaves,
                          no.variables,
                          contactp.length,
                          site.index,
                          commitInterval,
                          batchNo,
                          simNo,
                          locationSaveResults,
                          iterationID.vector) {

  # create empty dgTMatrix to record site state at each step within the specified commit interval
  allStates.matrix <- as(object = as.matrix(allStates.table[((no.variables + 1):(no.variables + contactp.length)),]),
                         Class = "dgTMatrix")

  simStates.longTable <- data.frame("siteID" = as.integer(site.index[(allStates.matrix@i + 1)] + 1),
                                    "state" = as.integer(allStates.matrix@x),
                                    "timeID" = as.integer(allStates.matrix@j + ((numberFullSaves - 1) * commitInterval)),
                                    "simNo" = as.integer(allStates.table[3,])[allStates.matrix@j + 1])

  simTimes.longTable <- data.frame("timeID" = as.integer(iterationID.vector + ((numberFullSaves - 1) * commitInterval)),
                                   "simNo" = as.integer(allStates.table[3,])[iterationID.vector],
                                   "tdiff" = as.numeric(allStates.table.t[1,])[iterationID.vector],
                                   "t" = as.numeric(allStates.table.t[2,])[iterationID.vector])

  save(simStates.longTable,
       simTimes.longTable,
       file = paste(locationSaveResults,"/FullDetails/batchNo-",batchNo,"_simNo-",simNo,"_NoCommits-",numberFullSaves,".RData",sep=""),
       compress=FALSE)
}
