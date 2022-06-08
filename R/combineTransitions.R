#' combineTransitionRates
#'
#' Append the contents of `list_append` list elements to the corresponding list elements in
#' `list_base` using `c`. Note, that list elements should be in the order specified within the
#' inputs.
#'
#' Note: in aquanet-mod, this function is called multiple times exclusively within the `update_rate`
#' function.
#'
#' TODO: Update function names
#'
#' @param list_append (class list) of length at least 4 containing the following at list positions
#' 1, 2, 3, and 4:
#' 1. (class numeric) vector of transition types.
#' 2. (class integer) vector of sites subject to transition.
#' 3. (class numeric) vector of transition rates (transmission probability).
#' 4. (class integer) vector of source sites (of disease in case of transmission).
#' contact.
#'
#' @param list_base (class list) of length 4 with either empty or populated list positions
#' containing the following at list positions 1, 2, 3, and 4:
#' 1. (class numeric) vector of transition types.
#' 2. (class integer) vector of sites subject to transition.
#' 3. (class integer) vector of transition rates (transmission probability).
#' 4. (class numeric) vector of source sites (of disease in case of transmission).
#'
#' @return (class list) of length 4 containing the following concatenated information from
#' `list_append` and `list_base`:
#' 1. (class numeric) vector of transition types.
#' 2. (class integer) vector of sites subject to transition.
#' 3. (class integer) vector of transition rates (transmission probability).
#' 4. (class numeric) vector of source sites (of disease in case of transmission).
#'
#' @export
#'
combineTransitionRates <- function(list_append, list_base) {
  # concatenate (1) transition type, (2) transition site, (3) transisiton rate and (4) source site
  list_base[[1]] <- c(list_base[[1]], list_append[[1]])
  list_base[[2]] <- c(list_base[[2]], list_append[[2]])
  list_base[[3]] <- c(list_base[[3]], list_append[[3]])
  list_base[[4]] <- c(list_base[[4]], list_append[[4]])

  # return concatenated transition information list
  return(list_base)
}
