% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epidemicSize.R
\name{epidemicSize}
\alias{epidemicSize}
\title{epidemicSize}
\usage{
epidemicSize(results)
}
\arguments{
\item{results}{(class data.frame) A data frame created from \code{loadResultsSummary}
Contains:
\enumerate{
\item \code{batch_no} the batch number
\item \code{k} the run number within the batch
\item \code{t} the model time
\item \code{t_diff} the difference between the current time and the time at the previous timestep
(the duration of the previous timestep)
\item \code{sim_no} the simulation number
\item \code{rate_type} the transition type
\item \code{no_controlled_catchments} the number of catchments under controls in that timestep
\item \code{cumulative_no_infected_sites} the cumulative number of infected sites in the simulation
Plus the number of sites in each state (1:39) plus redundancy (40:42)
}}
}
\value{
(class data.table) summary of epidemic infections (measured in number of sites infected):
\enumerate{
\item \code{mean_infections} the mean epidemic infections for the scenario
\item \code{sd_infections} the standard deviation of the epidemic infections for the scenario
\item \code{min_infections} minimum epidemic infections
\item \code{max_infections} maximum epidemic infections
\item \code{median_infections} medium epidemic infections
\item \code{q05_infections} the 5\% quartile for epidemic infections
\item \code{q95_infections} the 95\% quartile for epidemic infections
\item \code{percent_die_out} the percentage of simulations for which the epidemic did not
infect more than 5 sites in total
\item \code{no_iter} number of iterations/simulations run
}
}
\description{
epidemicSize
}