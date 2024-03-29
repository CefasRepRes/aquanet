% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listRatesSusceptibleRiskContacts.R
\name{listRatesSusceptibleRiskContacts}
\alias{listRatesSusceptibleRiskContacts}
\title{listRatesSusceptibleRiskContacts}
\usage{
listRatesSusceptibleRiskContacts(
  spmatrix_risk_contacts,
  state_vector,
  trans_type
)
}
\arguments{
\item{spmatrix_risk_contacts}{(class dgCMatrix, Matrix package) sparse matrix containing live
fish movements contact probability adapted to identify only contacts between sites that present a
risk of spread of pathogen. In AquaNet-Mod movements between sites in the same catchment are
filtered depending on catchment controls input into \code{aquanet::excludeWithinCatchmentMovements()}.

Source sites that are infected but cannot transport fish off site due to movement restrictions
have their contact probabilities converted to 0 as they cannot form 'at risk' contacts.
Additionally. sites that are uninfected with or without movement restrictions have a probability
of 0. Receiving sites that cannot transport fish on site due to movement restrictions also have
their contact probabilities converted to 0 as they cannot form 'at risk' contacts. At risk
contacts occur between sites that are infected with no restrictions on movement off site and
receiving sites with no restrictions on site. (Note: initially defined within
\code{aquanet::updateRates} function of AquaNet-Mod. In AquaNet-Mod this is one of the outputs
from \code{aquanet::excludeWithinCatchmentMovements()}).}

\item{state_vector}{(class numeric) numeric binary vector of length number of sites containing
information on whether each site is in an 1 = infected or 0 = susceptible state.
(Note: created within the \code{aquanet::simulationCode} function for loop).}

\item{trans_type}{(class numeric) number stating transition type.}
}
\value{
(class list) of length 5 containing:
\enumerate{
\item (class numeric) numeric vector of transition types (input \code{trans_type}) with length 'number of
sites with contact probability greater than 0 where the receiving site is in a susceptible state'.
\item (class integer) integer vector of sites subject to transition (positions/coordinate x of sites
with risk contact at susceptible sites \code{(spmatrix_risk_contacts@i)}) with length 'number of
sites with contact probability greater than 0 where the receiving site is in a susceptible state'.
\item (class numeric) numeric vector of transition rates/transmission probability (the probability
of risk contact at sites connected to susceptible sites. \code{(spmatrix_risk_contacts@x)}) with
length 'number of sites with contact probability greater than 0 where the receiving site is in a
susceptible state'.
\item (class integer) numeric vector of source sites (positions/coordinate y of sites with risk
contact at susceptible sites \code{(spmatrix_risk_contacts@j)}) with length 'number of sites with
contact probability greater than 0 where the receiving site is in a susceptible state'.
\item (class integer) number of sites that have a risk contact probability above 0 where the
receiving site is in a susceptible state.
}
}
\description{
This function lists transition rates for sites that can be infected via the LFM disease
transmission route of AquaNet-Mod (see details).
}
\details{
Extract information from risk contact matrix related to the sites that have the potential to be
infected (i.e. contact probability is greater than 0 and receiving sites are in a susceptible
state \code{state_vector[site] == 0}). Output information includes receiving siteID, probability of
contact, source siteID, and the number of contacts that pose a risk to receiving sites. A
transition rate vector is also produced for these sites using the \code{trans_type} input parameter.

Note: this function is called within \code{aquanet::updateRates} and \code{aquanet::calcRiverTransmission}
of AquaNet-Mod.
}
