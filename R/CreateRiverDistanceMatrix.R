CreateRiverDistanceMatrix <- function(filepath_river_distances, out_createContactProbabilityMatrix) {
  # create vector of sites in the same order as the adjacency matrix
  vector_sites <- as.numeric(out_createContactProbabilityMatrix[[3]]@Dimnames[[1]])

  # extract number of sites from the contact probability matrix
  n_sites <- out_createContactProbabilityMatrix[[1]]

  # load csv of site to site distances through the river network (generated with GIS tool)
  river_distances <- read.csv(file = filepath_river_distances, stringsAsFactors = FALSE)

  # define river distance origin and destination columns
  river_distances$Origin.SiteID <- regmatches(x = river_distances$Name, m = regexpr(perl = TRUE, text = river_distances$Name, pattern = "^[0-9]+"))
  river_distances$Dest.SiteID <- regmatches(x = river_distances$Name, m = regexpr(perl = TRUE, text = river_distances$Name, pattern = "[0-9]+$"))

  # retain only site to site distances where both the origin and destination site ID are present in the contact probability matrix
  river_distances <- subset(river_distances, Origin.SiteID %in% as.character(vector_sites) & Dest.SiteID %in% as.character(vector_sites))

  # remove site to site distances where the distance is zero
  river_distances_rm0 <- river_distances[river_distances$Total_Length > 0,]

  # convert site to site river distances to probabilities correcting for probabilities below zero
  river_distances_rm0$calcProb <- (0.005 * ((40 - (river_distances_rm0$Total_Length / 1000))/39))
  river_distances_rm0$calcProb <- ifelse(river_distances_rm0$calcProb < 0, 0, river_distances_rm0$calcProb)

  # extract only origin and destination sites from river distances
  riverDistance.table.noZeros.edgeList <- river_distances_rm0[ ,c("Origin.SiteID", "Dest.SiteID")]

  # express each siteID as a factor
  # levels assigned based on the site's position within the original contact matrix
  riverDistance.table.noZeros.edgeList$Origin.SiteID <- factor(x = riverDistance.table.noZeros.edgeList$Origin.SiteID,
                                                               levels = vector_sites)

  riverDistance.table.noZeros.edgeList$Dest.SiteID <- factor(x = riverDistance.table.noZeros.edgeList$Dest.SiteID,
                                                             levels = vector_sites)

  # Create an edge list, where each site is expressed in terms of it's position, within the original adjacency matrix
  riverDistance.table.noZeros.edgeList$Origin.Matrix.Pos <- as.numeric(riverDistance.table.noZeros.edgeList$Origin.SiteID)
  riverDistance.table.noZeros.edgeList$Dest.Matrix.Pos <- as.numeric(riverDistance.table.noZeros.edgeList$Dest.SiteID)

  # Create an empty matrix, to store distances between sites, along the river
  riverDistance.matrix <- Matrix::Matrix(data = 0,
                                nrow = n_sites,
                                ncol = n_sites,
                                dimnames = list(as.character(vector_sites),
                                                as.character(vector_sites)))

  # Identify positions within the matrix which correspond to contacts, and identify the contacts with '1'
  riverDistance.matrix[cbind(riverDistance.table.noZeros.edgeList$Origin.Matrix.Pos,
                             riverDistance.table.noZeros.edgeList$Dest.Matrix.Pos)] <- river_distances_rm0$calcProb

  riverDistance.matrix <- methods::as(riverDistance.matrix, 'dgTMatrix')

  return(list(river_distances_rm0, riverDistance.matrix))
}
