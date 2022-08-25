#' stateCount
#'
#' Returns a tally of the number of times a state appears in the summary table.
#'
#' Useful for checking you have the correct outputs from a scenario run.
#'
#' @param no_per_state_wide (class data.frame) returned from `loadResultsSummary`
#'
#' @return (class data.frame) sum_per_state
#' @export
#'
stateCount <- function(no_per_state_wide){
  sum_per_state <- as.data.frame(colSums(no_per_state_wide[,-c(1:8)]))
  sum_per_state$state <- 1:42
  colnames(sum_per_state)[1] <- "count"
  return(sum_per_state)
}
