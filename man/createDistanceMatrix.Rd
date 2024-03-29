% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createDistanceMatrix.R
\name{createDistanceMatrix}
\alias{createDistanceMatrix}
\title{createDistanceMatrix}
\usage{
createDistanceMatrix(
  graph,
  filename_site_catchments,
  crs_epsg,
  sdm_max_dist,
  sdm_rate_gamma,
  sdm_scalar_lambda
)
}
\arguments{
\item{graph}{(class igraph) graph of connections/movements between sites produced with iGraph
in '03_CreateContactNetwork.R' of AquaNet-mod.}

\item{filename_site_catchments}{(class string) string containing the file path and file name for
.csv containing information about site location (easting and northing) and which catchment each
site resides in.}

\item{crs_epsg}{(class numeric) 4-5 digit epsg code stating the coordinate reference system (crs)
to use for projecting the data.}

\item{sdm_max_dist}{(class numeric) maximum distance (in km) over which short distance
mechanical (SDM) transmission can occur.}

\item{sdm_rate_gamma}{(class numeric) daily short distance mechanical (SDM) transmission rate
over 0 km.}

\item{sdm_scalar_lambda}{(class numeric) local scalar of short distance mechanical (SDM)
transmission.}
}
\value{
(class list) of length 3 containing:
\enumerate{
\item (class matrix array) a matrix of site to site distances.
\item (dgTMatrix, Matrix package) a matrix of distance-based transmission probabilities.
\item (class data frame, sf package) input data frame of site catchment locality.
}
}
\description{
This function creates a sparse matrix for distance-related transmission probabilities between
sites for disease transmission via SDM route of AquaNet-Mod (see details).
}
\details{
Using the connectivity igraph and site to catchment information .csv create a matrix of distances
between these sites calculated using the sf package and user input coordinate reference system.
Additionally, filter these distances to remove self loops and distances greater than
sdm_max_dist m (assume greater than sdm_max_dist m constitutes negligible transmission risk).
Convert distances to a probability of transmission with the following equation
\code{sdm_rate_gamma * exp(-(distance)^2 * sdm_scalar_lambda)}. Convert probabilities that are less
than 0 or equal to sdm_rate_gamma to 0. Return a list containing site-site distance matrix,
site-site distance-based transmission probability matrix and site-catchment data frame. Note:
matrices produced have the same reorder distance matrix, so that it is in the same site order
as the contact matrix (graph).
}
