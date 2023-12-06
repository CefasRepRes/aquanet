#' loadRiverDistances
#'
#' River distances between sites are loaded from .csv whose file name is specified as input `filepath_river_distances`.
#' Origin and destination site IDs are extracted, and the data subsetted to retain only connections
#' where both origin and destination site are present in the live fish movements contact matrix
#' (generated with igraph).
#'
#' @param filepath_river_distances (class string) string containing the file path and file name for
#' .csv containing information about distances between sites via river network proximity and
#' connectivity generated with GIS tool.
#'
#' @param out_createContactProbabilityMatrix (class list) of length 3 containing:
#' 1. (class integer) number of sites in movements matrix (integer).
#' 2. (class dgCMatrix, Matrix package) movements matrix.
#' 3. (class dgCMatrix, Matrix package) probability of movements matrix with top sites zeroed.
#'
#' @return (class data frame) data frame containing the site origin and destination IDs for the river network.
#' @export
#'
loadRiverDistances <- function(filepath_river_distances,
                               out_createContactProbabilityMatrix){
  # create vector of sites in the same order as the adjacency matrix
  vector_sites <- out_createContactProbabilityMatrix[["matrix_movements_prob"]]@Dimnames[[1]]

  # load csv of site to site distances through the river network (generated with GIS tool)
  river_distances <- read.csv(file = filepath_river_distances, stringsAsFactors = FALSE)

  # define river distance origin and destination columns
  river_distances$Origin.SiteID <- regmatches(x = river_distances$Name,
                                              m = regexpr(perl = TRUE, text = river_distances$Name,
                                                          pattern = "^.*(?=(\ - ))"))

  river_distances$Dest.SiteID <- regmatches(x = river_distances$Name,
                                            m = regexpr(perl = TRUE, text = river_distances$Name,
                                                        pattern = "(?<= - ).*$"))

  # retain only site to site distances where both origin and destination are present in the contact
  # probability matrix
  river_distances <- subset(river_distances,
                            river_distances$Origin.SiteID %in% as.character(vector_sites) &
                              river_distances$Dest.SiteID %in% as.character(vector_sites))

  # remove site to site distances where the distance is zero
  river_distances_df <- river_distances[river_distances$Total_Length > 0, ]

  # return the river distances object
  return(river_distances_df)
}
