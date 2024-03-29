% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateRates.R
\name{updateRates}
\alias{updateRates}
\title{updateRates}
\usage{
updateRates(
  control_matrix,
  state_vector,
  farm_vector,
  culling_vector,
  site_indices,
  catchment_movements,
  movements_prob,
  movements_prob_top_sites_removed,
  river_prob,
  site_distances_prob,
  run_time_params,
  non_peak_season,
  contact_tracing,
  remove_top_sites,
  sites_states_cumulative,
  n_infections_remove_top_sites,
  disease_controls,
  river_distances_df,
  site_details
)
}
\arguments{
\item{control_matrix}{(class matrix) matrix containing 7 columns depicting different control
states and rows depicting whether each sites is 1 = in the specified control state or 0 = not in
the specified control state.}

\item{state_vector}{(class numeric)  numeric binary vector of length 'number of sites' containing
information about the state of each site in relation to a condition. This state vector should
specify whether a site is in an 1 = infected or 0 = susceptible state. (Note: created within the
\code{aquanet::simulationCode} function for loop).}

\item{farm_vector}{(class numeric) numeric binary vector of length number of sites containing
information on whether each site 1 = is a farm or 0 = is not a farm.}

\item{culling_vector}{(class numeric) numeric binary vector of length number of sites containing
information on whether each site 1 = can be culled or 0 = cannot be culled.}

\item{site_indices}{(class numeric) vector of 0-based site indices of length 'number of sites'.}

\item{catchment_movements}{(class list) of length 7 produced using the containing objects
related to catchment-level movements:
\enumerate{
\item (class dgCMatrix, Matrix package) sparse matrix containing details of site to catchment
relationships.
\item (class lgCMatrix, Matrix package) logical matrix contacting details of sites within the same
catchment.
\item (class dgeMatrix, Matrix package) matrix of length number of catchments showing which
catchments were under controls in the previous time step.
\item (class dgTMatrix, Matrix package) sparse matrix containing contacts to exclude.
\item (class numeric) number selecting catchment level controls to apply (0 = allows movements
within the same catchments, 1 = allows movements within or between infected catchments, and 2 =
allows no movements by any of the sites within an infected catchment, "None" means there are no
catchment level controls).
\item (class logical) logical vector of length number of sites stating if sites are under secondary
levels of control.
\item (class numeric) the total number of catchments under controls.
}}

\item{movements_prob}{(class dgTMatrix, Matrix package) a matrix of live fish movement
probabilities between sites generated using the \code{aquanet::createContactProbabilityMatrix()}
function.}

\item{movements_prob_top_sites_removed}{(class dgTMatrix, Matrix package) a matrix of live fish
movement probabilities between sites, but with the top most connected sites in the network
removed. Generated using the \code{aquanet::createContactProbabilityMatrixTopSitesRemoved()}function.}

\item{river_prob}{(class dgTMatrix, Matrix package) a matrix of river connectivity distance-based
transmission probabilities generated using the \code{aquanet::createRiverDistanceProbabilityMatrix()}
function.}

\item{site_distances_prob}{(class dgTMatrix, Matrix package) a matrix of distance-based
transmission probabilities generated using the \code{aquanet::createDistanceMatrix()} function.}

\item{run_time_params}{(class data frame) of model run time parameters imported from original
parameter file which is subsequently split into model set up and model run time parameters. Data
frame contains probabilities from the scenarios of interest for listing transition rates.}

\item{non_peak_season}{(class logical) logical indicating whether the current time step in the
model is within non peak season where pathogen transmission is lower.}

\item{contact_tracing}{(class logical) vector of length 1 indicating whether or not contact
tracing is taking place.}

\item{remove_top_sites}{(class logical) vector of length 1 indicating whether or not the removal
of the most connected sites in the network should take place.}

\item{sites_states_cumulative}{(class numeric) numeric binary vector indicating whether a site is
infected (1) or susceptible (0).}

\item{n_infections_remove_top_sites}{(class numeric) vector of length 1. After the cumulative
number of infected sites exceeds this number, switch to using the top sites removed contact
probability matrix.}

\item{disease_controls}{(class logical) vector of length 1 indicating whether or not any disease
control measures should take place.}

\item{river_distances_df}{(class data frame) a data frame of distances between sites
along the river network. Created using the GIS tool.}

\item{site_details}{(class data frame) a data frame of site and model IDs, locations and whether
or not the site is tidal. Created using \code{aquanet::mergeGraphMetaWithCatchmentLocation}.}
}
\value{
(class list) of length 3 containing:
\enumerate{
\item (class list) of length 4 containing transition rates:
1.1. (class numeric) vector of transition types.
1.2. (class integer) vector of sites subject to transition.
1.3. (class integer) vector of transition rates (transmission probability).
1.4. (class numeric) vector of source sites (of disease in case of transmission).
\item (class list) of length 7 containing movements_within_catchment input that has been updated #
with to \code{aquanet::excludeWithinCatchmentMovements()} function inside \code{aquanet::updateRates()}.
Updated elements include 3, 4, 6, and 7 (element 6 only changes if different catchments are under
control compared to previous time step).
\item (class logical) logical vector of length number of sites containing information on whether a
site is subject to any kind of movement restrictions.
}
}
\description{
This function calculates the rates that each site can transition between different states in
AquaNet-Mod (see details).
}
\details{
Define logical vectors of sites depending on different combinations of  movement restrictions and
site states specified within an input \code{control_matrix}. This includes sites where movements are
restricted, where transport of animals on and off site is restricted, and sites where spread on
and off site are not possible due to restrictions.

Next, vectors of different site types are defined based on site states, site types, infection
status and the ability to cull to achieve recovered status at sites. Using this information, live
fish movements that have the potential to spread (e.g. a pathogen) are identified, and within
catchment movements are corrected to remove within catchment movements.

Epidemic rates are then calculated, including: site infection rate, farm recovery rate, fishery
latency rate, site clearing rate, site disinfection rate, site contact tracing rate, site
detection rate, farm fallow rate. These are combined and output.

If the current time step is within the peak transmission season specified within the inputs then
the rate at which latent sites become reinfected, the rate at which spread occurs via river
network connectivity and the rate of fomite transmission are calculated and also combined.

Finally, if there are susceptible sites without restrictions preventing spread on site, a rate of
random spillover into these sites is also calculated and combined with the outputs.
}
