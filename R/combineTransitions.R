combineTransitionRates <- function(transition.objects, transition.rates) {
  # concatenate (1) transition type, (2) transition site, (3) transisiton rate and (4) source site
  transition.rates[[1]] <- c(transition.rates[[1]], transition.objects[[1]])
  transition.rates[[2]] <- c(transition.rates[[2]], transition.objects[[2]])
  transition.rates[[3]] <- c(transition.rates[[3]], transition.objects[[3]])
  transition.rates[[4]] <- c(transition.rates[[4]], transition.objects[[4]])

  # return concatenated transition information list
  return(transition.rates)
}
