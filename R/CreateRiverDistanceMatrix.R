CreateRiverDistanceMatrix <- function(filepath_river_distances, graph.contactp.objects, ListModelSetupParameters) {
  # List of sites, in the same order as the adjacency matrix
  graph.contactp.listSites <- as.numeric(graph.contactp.objects[[3]]@Dimnames[[1]])

  # Store the number of sites in the contactp matrix
  contactp.length <- graph.contactp.objects[[1]]

  River.Transmission_Const <- 1 / as.numeric(ListModelSetupParameters[c('River.Downstream_Transmission_Const','River.UpDownstream_Transmission_Const')])
  River.Transmission_Current_Speed <- as.numeric(ListModelSetupParameters[c('River.Downstream.Current_speed','River.UpDownstream.Current_speed')])

  # Import the table of site to site distances, through the river network
  riverDistance.table <- read.csv(file = filepath_river_distances, stringsAsFactors = FALSE)

  riverDistance.table$Origin.SiteID <- regmatches(x = riverDistance.table$Name, m = regexpr(perl = TRUE, text = riverDistance.table$Name, pattern = "^[0-9]+"))
  riverDistance.table$Dest.SiteID <- regmatches(x = riverDistance.table$Name, m = regexpr(perl = TRUE, text = riverDistance.table$Name, pattern = "[0-9]+$"))

  riverDistance.table <- subset(riverDistance.table, Origin.SiteID %in% as.character(graph.contactp.listSites) & Dest.SiteID %in% as.character(graph.contactp.listSites))

  # Exclude site to site distances, where the distance is zero
  riverDistance.table.noZeros <- riverDistance.table[riverDistance.table$Total_Length > 0,]

  riverDistance.table.noZeros$calcProb <- (0.005 * ((40 - (riverDistance.table.noZeros$Total_Length / 1000))/39))
  riverDistance.table.noZeros$calcProb <- ifelse(riverDistance.table.noZeros$calcProb < 0, 0, riverDistance.table.noZeros$calcProb)


  # Simplify table, including only the siteID
  riverDistance.table.noZeros.edgeList <- riverDistance.table.noZeros[,c('Origin.SiteID','Dest.SiteID')]

  # Express each site as a factor
  # Levels are assigned based on the site's position within the original adjacency matrix
  riverDistance.table.noZeros.edgeList$Origin.SiteID =
    factor(x = riverDistance.table.noZeros.edgeList$Origin.SiteID,
           levels = graph.contactp.listSites)

  riverDistance.table.noZeros.edgeList$Dest.SiteID =
    factor(x = riverDistance.table.noZeros.edgeList$Dest.SiteID,
           levels = graph.contactp.listSites)

  # Create an edge list, where each site is expressed in terms of it's position, within the original adjacency matrix
  riverDistance.table.noZeros.edgeList$Origin.Matrix.Pos <-
    as.numeric(riverDistance.table.noZeros.edgeList$Origin.SiteID)

  riverDistance.table.noZeros.edgeList$Dest.Matrix.Pos <-
    as.numeric(riverDistance.table.noZeros.edgeList$Dest.SiteID)

  # Create an empty matrix, to store distances between sites, along the river
  riverDistance.matrix <- Matrix::Matrix(data = 0,
                                nrow = contactp.length,
                                ncol = contactp.length,
                                dimnames = list(as.character(graph.contactp.listSites),
                                                as.character(graph.contactp.listSites)))

  # Identify positions within the matrix which correspond to contacts, and identify the contacts with '1'
  riverDistance.matrix[cbind(riverDistance.table.noZeros.edgeList$Origin.Matrix.Pos,
                             riverDistance.table.noZeros.edgeList$Dest.Matrix.Pos)] <- riverDistance.table.noZeros$calcProb

  riverDistance.matrix <- methods::as(riverDistance.matrix, 'dgTMatrix')

  return(list(riverDistance.table.noZeros, riverDistance.matrix))
}
