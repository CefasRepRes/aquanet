CreateRiverDistanceMatrix <- function(filepath_river_distances, out_createContactProbabilityMatrix) {
  # create vector of sites in the same order as the adjacency matrix
  vector_sites <- as.numeric(out_createContactProbabilityMatrix[[3]]@Dimnames[[1]])

  # Store the number of sites in the contactp matrix
  n_sites <- out_createContactProbabilityMatrix[[1]]

  # Import the table of site to site distances, through the river network
  river_distances <- read.csv(file = filepath_river_distances, stringsAsFactors = FALSE)

  river_distances$Origin.SiteID <- regmatches(x = river_distances$Name, m = regexpr(perl = TRUE, text = river_distances$Name, pattern = "^[0-9]+"))
  river_distances$Dest.SiteID <- regmatches(x = river_distances$Name, m = regexpr(perl = TRUE, text = river_distances$Name, pattern = "[0-9]+$"))

  river_distances <- subset(river_distances, Origin.SiteID %in% as.character(vector_sites) & Dest.SiteID %in% as.character(vector_sites))

  # Exclude site to site distances, where the distance is zero
  riverDistance.table.noZeros <- river_distances[river_distances$Total_Length > 0,]

  riverDistance.table.noZeros$calcProb <- (0.005 * ((40 - (riverDistance.table.noZeros$Total_Length / 1000))/39))
  riverDistance.table.noZeros$calcProb <- ifelse(riverDistance.table.noZeros$calcProb < 0, 0, riverDistance.table.noZeros$calcProb)


  # Simplify table, including only the siteID
  riverDistance.table.noZeros.edgeList <- riverDistance.table.noZeros[,c('Origin.SiteID','Dest.SiteID')]

  # Express each site as a factor
  # Levels are assigned based on the site's position within the original adjacency matrix
  riverDistance.table.noZeros.edgeList$Origin.SiteID <-
    factor(x = riverDistance.table.noZeros.edgeList$Origin.SiteID,
           levels = vector_sites)

  riverDistance.table.noZeros.edgeList$Dest.SiteID <-
    factor(x = riverDistance.table.noZeros.edgeList$Dest.SiteID,
           levels = vector_sites)

  # Create an edge list, where each site is expressed in terms of it's position, within the original adjacency matrix
  riverDistance.table.noZeros.edgeList$Origin.Matrix.Pos <-
    as.numeric(riverDistance.table.noZeros.edgeList$Origin.SiteID)

  riverDistance.table.noZeros.edgeList$Dest.Matrix.Pos <-
    as.numeric(riverDistance.table.noZeros.edgeList$Dest.SiteID)

  # Create an empty matrix, to store distances between sites, along the river
  riverDistance.matrix <- Matrix::Matrix(data = 0,
                                nrow = n_sites,
                                ncol = n_sites,
                                dimnames = list(as.character(vector_sites),
                                                as.character(vector_sites)))

  # Identify positions within the matrix which correspond to contacts, and identify the contacts with '1'
  riverDistance.matrix[cbind(riverDistance.table.noZeros.edgeList$Origin.Matrix.Pos,
                             riverDistance.table.noZeros.edgeList$Dest.Matrix.Pos)] <- riverDistance.table.noZeros$calcProb

  riverDistance.matrix <- methods::as(riverDistance.matrix, 'dgTMatrix')

  return(list(riverDistance.table.noZeros, riverDistance.matrix))
}
