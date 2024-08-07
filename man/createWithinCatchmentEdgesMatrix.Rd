% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createWithinCatchmentEdgesMatrix.R
\name{createWithinCatchmentEdgesMatrix}
\alias{createWithinCatchmentEdgesMatrix}
\title{createWithinCatchmentEdgesMatrix}
\usage{
createWithinCatchmentEdgesMatrix(graph)
}
\arguments{
\item{graph}{(class igraph) Graph of connections/movements between sites produced with iGraph
in '03_CreateContactNetwork.R' of AquaNet-mod. This includes both live fish movements and
Section 30 movements.}
}
\value{
(class list) of length 3 containing:
\enumerate{
\item (class lgCMatrix) logical matrix detailing within catchment connections.
\item (class matrix) edge matrix of vertex IDs within catchments.
\item (class matrix) matrix of source site and receiving site within catchment edges.
}
}
\description{
This function produces a logical matrix detailing within catchment connections (see details).
}
\details{
Extract connectivity matrix (graph) information at "withinCatchment" level to produce a logical
matrix of within catchment connections, a matrix of within catchment edges and a matrix of within
catchment edges by source (column 1) and receiving (column 2) site ID.
}
