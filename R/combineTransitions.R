combineTransitionRates <- function(list_append, list_base) {
  # concatenate (1) transition type, (2) transition site, (3) transisiton rate and (4) source site
  list_base[[1]] <- c(list_base[[1]], list_append[[1]])
  list_base[[2]] <- c(list_base[[2]], list_append[[2]])
  list_base[[3]] <- c(list_base[[3]], list_append[[3]])
  list_base[[4]] <- c(list_base[[4]], list_append[[4]])

  # return concatenated transition information list
  return(list_base)
}
