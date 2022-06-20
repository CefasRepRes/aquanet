commitResults = function(allStates.table, allStates.table.t, numberFullSaves) {
  allStates.matrix = as(object = as.matrix(allStates.table[((no.variables + 1):(no.variables + contactp.length)),]), Class = "dgTMatrix")

  simStates.longTable = data.frame(as.integer(site.index[(allStates.matrix@i + 1)] + 1),
                                   as.integer(allStates.matrix@x),
                                   as.integer(allStates.matrix@j + ((numberFullSaves - 1) * commitInterval)),
                                   as.integer(allStates.table[3,])[allStates.matrix@j + 1])

  colnames(simStates.longTable) = c('siteID','state','timeID','simNo')

  simTimes.longTable = data.frame(as.integer(iterationID.vector + ((numberFullSaves - 1) * commitInterval)),
                                  as.integer(allStates.table[3,])[iterationID.vector],
                                  as.numeric(allStates.table.t[1,])[iterationID.vector],
                                  as.numeric(allStates.table.t[2,])[iterationID.vector])

  colnames(simTimes.longTable) = c('timeID','simNo','tdiff','t')


  save(simStates.longTable, simTimes.longTable, file = paste(locationSaveResults,"/batch_results/states-batchNo-",batchNo,"_simNo-",simNo,".RData",sep=""),compress=FALSE)
}
