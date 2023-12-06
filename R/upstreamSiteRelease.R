#' upstreamSiteRelease
#'
#' This function is used when the catchment-level controls include the release of
#' sites upstream of infected sites. It identifies "safe sites" upstream of the infected
#' site so they may be removed from controls.
#'
#'
#' @param river_distances_df (class data frame) a data frame of distances between sites
#' along the river network. Created using the GIS tool.
#' @param move_restricted_sites (class logical) logical vector of length 'number of sites' that
#' states whether movements at this site are currently restricted (TRUE) or unrestricted (FALSE).
#' @param sites_tidal (class data frame) logical data frame of length 'number of sites' that
#' states whether this site is located within a tidal boundary (TRUE) or not (FALSE).
#' (Note: created in the `aquanet::updateRates` function of aquanet-mod).
#' @param sites_unique (class vector) a vector of unique site IDs.
#' @param sites_controlled (class vector) a vector of sites currently under catchment-level
#' controls (0 = FALSE, 1 = TRUE).
#'
#' @return (class vector) a vector of sites indicating whether or not they are under
#' catchment-level controls (0 = FALSE, 1 = TRUE).
#'
#' @export
#'
upstreamSiteRelease <- function(river_distances_df,
                                move_restricted_sites,
                                sites_tidal,
                                sites_unique,
                                sites_controlled){
  # satisfy "no global binding" warning
  infected <- Dest.SiteID <- sites <- tidal <- NULL

  # create a data frame of infected and detected sites
  infected_df <- data.frame(site = sites_unique,
                            infected = move_restricted_sites) %>% data.table()
  infected_df <- infected_df[infected == TRUE]

  # filter sites upstream of infected sites
  river_distances_df <- data.table(river_distances_df)
  upstream_clear <- river_distances_df[(Dest.SiteID %in% infected_df$site)]
  upstream_clear_sites <- data.frame(sites = c(upstream_clear$Origin.SiteID, upstream_clear$Dest.SiteID)) %>% data.table()

  # filter tidal sites
  tidal_df <- sites_tidal[tidal == TRUE]

  # remove infected and tidal sites
  upstream_clear_sites <- upstream_clear_sites[!(sites %in% infected_df$site)]
  upstream_clear_sites <- upstream_clear_sites[!(sites %in% tidal_df$site)]
  upstream_clear_sites <- as.vector(unique(upstream_clear_sites$sites))

  # make into data frame with correct positioning
  upstream_df <- data.frame(site = sites_unique,
                            upstream = NA)
  upstream_df$upstream <- ifelse(upstream_df$site %in% upstream_clear_sites, 1, 0)
  upstream_vector <- as.vector(upstream_df$upstream)

  # Return the downstream_sites
  return(upstream_vector)
}
