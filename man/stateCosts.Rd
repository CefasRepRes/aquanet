% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stateCosts.R
\name{stateCosts}
\alias{stateCosts}
\title{stateCosts}
\usage{
stateCosts(data, state, site_types)
}
\arguments{
\item{data}{(class data.frame) output of timePerStage. Contains the site ID,
state, simulation number, total time, and the logical vector of site types.}

\item{state}{(class character) describes the state for which the costs will be
calculated. Can be:
\enumerate{
\item \code{fallow} in a fallow state (infected or uninfected)
\item \code{no_manage} infected in an unmanaged state
\item \code{contact_trace} in a contact tracing state (infected or uninfected)
\item \code{catchment_control} uninfected site under catchment-level controls
}}

\item{site_types}{(class list) a list of possible site types}
}
\value{
(class data.frame) sim_cost_summary
}
\description{
Calculates the total duration costs for each simulation across fallow, contact tracing,
catchment control and unmanaged disease scenarios. Returns a data frame containing
the simulation number and the cost incurred by sites being in that state.
}