% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epidemicDuration.R
\name{epidemicDuration}
\alias{epidemicDuration}
\title{epidemicDuration}
\usage{
epidemicDuration(results, summary = F)
}
\arguments{
\item{results}{(class data.frame) A data frame created from \code{aquanet::loadResultsSummary()} that
contains:
\enumerate{
\item \code{batch_no} the batch number.
\item \code{k} the run number within the batch.
\item \code{t} the model time.
\item \code{t_diff} the difference between the current time and the time at the previous time step (the
duration of the previous time step).
\item \code{sim_no} the simulation number.
\item \code{rate_type} the transition type.
\item \code{no_controlled_catchments} the number of catchments under controls in that time step.
\item \code{cumulative_no_infected_sites} the cumulative number of infected sites in the simulation.
Plus the number of sites in each state (1:39) plus redundancy (40:42).
}}

\item{summary}{(class logical) A logical to return either a summary (T) or the duration of the epidemic
per scenario (F). F is the default.}
}
\value{
(class data.table)

If summary = F, the simulation number \code{sim_no} and the epidemic duration \code{max_t}.

summary of epidemic duration (measured in days) that contains:
\enumerate{
\item \code{mean_duration} the mean epidemic duration for the scenario.
\item \code{sd_duration} the standard deviation of the epidemic duration for the scenario.
\item \code{min_duration} minimum epidemic duration.
\item \code{max_duration} maximum epidemic duration.
\item \code{median_duration} medium epidemic duration.
\item \code{q05_duration} the 5\% quartile for epidemic duration.
\item \code{q95_duration} the 95\% quartile for epidemic duration.
\item \code{percent_time_out} the percentage of simulations for which the epidemic did not last longer
than 5 years (1800 model days).
\item \code{no_iter} numer of iterations per simulation run.
}
}
\description{
This function summarises the simulation results to return statistics related to the duration of
the epidemic, including mean and median duration and percentage time out.
}
