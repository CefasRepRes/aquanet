#' upstreamSiteRelease
#'
#' This function is used when the catchment-level controls include the release of
#' sites upstream of infected sites.
#'
#' First, a vector of upstream sites is identified, where 0 = downstream and 1 =
#' upstream.
#' Second, upstream sites are removed from `downstream_sites_controlled`, which
#' gives the sites under catchment-level controls.
#' The output is then used to adjust the contact probability matrix.
#'
#' @param downstream_sites (class vector) a vector of downstream sites, created
#' by the `createRiverDistanceProbabilityMatrix` function.
#' @param sites_unique (class vector) a vector of unique site IDs.
#' @param sites_controlled (class vector) a vector of sites currently under catchment-level
#' controls (0 = FALSE, 1 = TRUE).
#'
#' @return (class vector) a vector of sites indicating whether or not they are under
#' catchment-level controls (0 = FALSE, 1 = TRUE).
#'
#' @export
#'
upstreamSiteRelease <- function(downstream_sites,
                                sites_unique,
                                sites_controlled){
  # create a vector of upstream sites
    # 0 = downstream, 1 = upstream
  upstream_df <- data.frame(site = sites_unique,
                                 upstream = NA)
  upstream_df$upstream <- ifelse(upstream_df$site %in% downstream_sites, 0, 1)
  upstream_vector <- as.vector(upstream_df$upstream)

  # remove from sites_controlled
  # correct to 0
  downstream_sites_controlled <- sites_controlled - upstream_vector
  downstream_sites_controlled[downstream_sites_controlled < 0] <- 0

  # Return the downstream_sites
  return(downstream_sites_controlled)
}
