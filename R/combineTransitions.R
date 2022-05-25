combineTransitions <- function(transition.objects, transition.rates) {
  # Type of transition
  transition.rates[[1]] <- c(transition.rates[[1]],transition.objects[[1]])
  # Site subject to transition
  transition.rates[[2]] <- c(transition.rates[[2]],transition.objects[[2]])
  # Transition rate
  transition.rates[[3]] <- c(transition.rates[[3]],transition.objects[[3]])
  # Source of disease (in the case of transmissions)
  transition.rates[[4]] <- c(transition.rates[[4]],transition.objects[[4]])

  return(transition.rates)
}
