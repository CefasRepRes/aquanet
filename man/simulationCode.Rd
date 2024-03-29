% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulationCode.R
\name{simulationCode}
\alias{simulationCode}
\title{simulationCode}
\usage{
simulationCode(
  runs,
  tmax,
  batch_num,
  run_time_params,
  non_peak_season_length,
  out_createContactProbabilityMatrix,
  out_createContactProbabilityMatrixTopSitesRemoved,
  out_createWithinCatchmentEdges,
  out_createCatchmentToSiteMatrix,
  out_createRiverDistanceProbabilityMatrix,
  out_createDistanceMatrix,
  farm_vector,
  n_states,
  n_initial_infections,
  type_catchment_controls,
  filepath_results,
  contact_tracing,
  remove_top_sites,
  n_infections_remove_top_sites,
  disease_controls,
  proportion_cullable,
  days_before_catchment_restock,
  river_distances_df,
  site_details
)
}
\arguments{
\item{runs}{(class numeric) number of runs.}

\item{tmax}{(class numeric) maximum amount of time in days that each simulation should run for.}

\item{batch_num}{(class numeric) batch number of parallel run.}

\item{run_time_params}{(class data frame) of model run time parameters imported from original
parameter file which is subsequently split into model set up and model run time parameters. Data
frame contains probabilities from the scenarios of interest for listing transition rates.}

\item{non_peak_season_length}{(class string) the amount of time in days that the non peak
transmission season lasts for (e.g. "90", "180").}

\item{out_createContactProbabilityMatrix}{(class list) of length 3 containing (1) number of sites
in (live fish) movements matrix (integer), (2) (live fish) movements matrix (dgCMatrix, Matrix
package), and (3) probability of (live fish) movements matrix (dgTMatrix, Matrix package).}

\item{out_createContactProbabilityMatrixTopSitesRemoved}{(class list) of length 3 containing (1)
number of sites in (live fish) movements matrix (integer), (2) (live fish) movements matrix
(dgCMatrix, Matrix package), and (3) probability of (live fish) movements matrix (dgTMatrix,
Matrix package). This object is created following the removal of the top most connected sites
in the network.}

\item{out_createWithinCatchmentEdges}{(class list) of length 3 containing (1) lgCMatrix (logical
matrix) detailing within catchment connections, (2) edge matrix of vertex IDs within catchments,
and (3) matrix of source site and receiving site within catchment edges.}

\item{out_createCatchmentToSiteMatrix}{(class list) of length 2 containing (1) data frame of site
to catchment information and (2) dgCMatrix sparse matrix containing site to catchment summary.}

\item{out_createRiverDistanceProbabilityMatrix}{(class list) of length 2 containing (1)
distances between sites on a river network (via river connectivity) connections of 0 distances
are removed (data frame) and (2) sparse matrix containing probability of transmission between
sites connected via the river network by river water (dgTMatrix).}

\item{out_createDistanceMatrix}{(class list) of length 3 containing (1) a matrix of site to site
distances (class matrix array), (2) a matrix of distance-based transmission probabilities
(dgTMatrix, Matrix package), and (3) input data frame of site catchment locality
(SpatialPointsDataFrame, sp package).}

\item{farm_vector}{(class numeric) numeric binary vector of length number of sites containing
information on whether each site 1 = is a farm or 0 = is not a farm.}

\item{n_states}{(class numeric) number of different combinations of states possible within the
model.}

\item{n_initial_infections}{(class numeric) number of farms to seed infection at.}

\item{type_catchment_controls}{(class numeric) tnumber selecting catchment level controls to
apply (0 = allows movements within the same catchments, 1 = allows movements within or between
infected catchments, and 2 = allows no movements by any of the sites within an infected
catchment, "None" means there are no catchment level controls).}

\item{filepath_results}{(class character) character vector containing paths to various results
directories for model run created in \code{aquanet-mod/code/RunModelCommandLine.R}.}

\item{contact_tracing}{(class logical) vector of length 1 indicating whether or not contact
tracing should take place.}

\item{remove_top_sites}{(class logical) vector of length 1 indicating whether or not the remova
of the most connected sites in the network should take place.}

\item{n_infections_remove_top_sites}{(class numeric) vector of length 1. After the cumulative
number of infected sites exceeds this number, switch to using the top sites removed contact
probability matrix.}

\item{disease_controls}{(class logical) vector of length 1 indicating whether or not
any disease control measures should take place.}

\item{proportion_cullable}{(class numeric) proportion of fisheries able to cull site.}

\item{days_before_catchment_restock}{(class numeric) number of days that all sites within a
catchment need to be in the post-fallow state before restocking.}

\item{river_distances_df}{(class data frame) a data frame of distances between sites
along the river network. Created using the GIS tool.}

\item{site_details}{(class data frame) a data frame of site and model IDs, locations and whether
or not the site is tidal. Created using \code{aquanet::mergeGraphMetaWithCatchmentLocation}.}
}
\value{
(class numeric) batch number \code{batch_num} and output_summary_states saved to
\code{filepath_results}.
}
\description{
This function contains the core code to run a simulation of AquaNet-Mod (see details).
}
\details{
In the first segments of the code, values are extracted from input parameters and empty variables
to populate with each simulation are created. The latter includes number of steps, operations,
saves, commit intervals and a \code{site_index}.

The for loop:

Then, for each run a simulation number is calculated, and various variables are reset including:
the time, time difference, transition type, catchment time vector, catchments with only sites in
post fallow state, time vector, control matrix, state vector, cumulative site states, infection
source vector, matrix of contacts to exclude, catchments controlled in the previous run, and
sites under control. A list of catchment movement objects is created. A vector of farms that are
able to cull is updated with inclusion of a random number of approximately 50\% fisheries assumed
to be able to cull.

A vector of farms where infection could be seeded is generated and then a farm is chosen at
random to seed infection and their infection status updated.

The while loop:

While the simulation time is below the maximum simulation time \code{tmax} determine which season it
is. Input seasonality information into \code{aquanet::updateRates()} function and extract the outputs.
Calculate a state number for each of the sites in the network and summarise number of sites in
each infection/control state.

Increment the number of operations and add the summary of states to the summary states table. If
the operation is one prior to the commit interval, extend the summary states table in size.

If there are no more infected sites within the network, break the while loop.

Choose a random weighted exponential distribution value to increment the simulation time by and
increase the number of steps.

Pick the next transmission event with \code{aquanet::doEvent()} and modify site's state accordingly.
Update variables given outputs.

Finally, after the for and while loop, save the summary states table.
}
