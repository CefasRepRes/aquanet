createRiverDistanceProbabilityMatrix <- function(filepath_river_distances, out_createContactProbabilityMatrix) {
  # create vector of sites in the same order as the adjacency matrix
  vector_sites <- as.numeric(out_createContactProbabilityMatrix[[3]]@Dimnames[[1]])

  # extract number of sites from the contact probability matrix
  n_sites <- out_createContactProbabilityMatrix[[1]]

  # load csv of site to site distances through the river network (generated with GIS tool)
  river_distances <- read.csv(file = filepath_river_distances, stringsAsFactors = FALSE)

  # define river distance origin and destination columns
  river_distances$Origin.SiteID <- regmatches(x = river_distances$Name,
                                              m = regexpr(perl = TRUE,
                                                          text = river_distances$Name,
                                                          pattern = "^[0-9]+"))

  river_distances$Dest.SiteID <- regmatches(x = river_distances$Name,
                                            m = regexpr(perl = TRUE,
                                                        text = river_distances$Name,
                                                        pattern = "[0-9]+$"))

  # retain only site to site distances where both the origin and destination site ID are present in the contact probability matrix
  river_distances <- subset(river_distances,
                            Origin.SiteID %in% as.character(vector_sites) &
                              Dest.SiteID %in% as.character(vector_sites))

  # remove site to site distances where the distance is zero
  river_distances_rm0 <- river_distances[river_distances$Total_Length > 0, ]

  # convert site to site river distances to probabilities correcting for probabilities below zero
  river_distances_rm0$calcProb <- (0.005 * ((40 - (river_distances_rm0$Total_Length / 1000))/39))

  river_distances_rm0$calcProb <- ifelse(river_distances_rm0$calcProb < 0,
                                         yes = 0,
                                         no = river_distances_rm0$calcProb)

  # create edge list of origin and destination river sites
  river_distances_rm0_edges <- river_distances_rm0[, c("Origin.SiteID", "Dest.SiteID")]

  # express each siteID as a factor
  # levels assigned based on the site's position within the original contact matrix
  river_distances_rm0_edges$Origin.SiteID <- factor(x = river_distances_rm0_edges$Origin.SiteID,
                                                    levels = vector_sites)

  river_distances_rm0_edges$Dest.SiteID <- factor(x = river_distances_rm0_edges$Dest.SiteID,
                                                  levels = vector_sites)

  # create edge list, where each site is expressed in terms of it's position, within the original adjacency matrix
  river_distances_rm0_edges$Origin.Matrix.Pos <- as.numeric(river_distances_rm0_edges$Origin.SiteID)
  river_distances_rm0_edges$Dest.Matrix.Pos <- as.numeric(river_distances_rm0_edges$Dest.SiteID)

  # create empty matrix to store river distances between sites
  matrix_river_distances_prob <- Matrix::Matrix(data = 0,
                                         nrow = n_sites,
                                         ncol = n_sites,
                                         dimnames = list(as.character(vector_sites),
                                                         as.character(vector_sites)))

  # insert site to site transmission probabilities via river to the matrix
  matrix_river_distances_prob[cbind(river_distances_rm0_edges$Origin.Matrix.Pos,
                             river_distances_rm0_edges$Dest.Matrix.Pos)] <- river_distances_rm0$calcProb

  # return list containing (1) data frame of river distances and
  # (2) river transmission probability matrix
  return(list(river_distances_rm0, matrix_river_distances_prob))
}
