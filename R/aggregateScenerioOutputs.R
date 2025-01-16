
#' aggregateScenerioOutputs

#'This function takes outputs from 'importAndCondense' and aggregates results by site, resulting in a final data set which has proportion of time spent in each state, proportion of transmissions for each site, along with information on the site category.

#' Possible mechanisms for transmissions are:
#' 1. LFM = live fish movement
#' 2. RB = river-based transmission
#' 3. SDM = short-distance mechanical
#' 4. DIM = distance-independent mechanical (random spillover)
#'
#' Possible states are:
#'  0 = Uninfected
#'  1 = Infected
#'  2 = Uninfected with secondary controls
#'  3 = Infected with secondary controls
#'
#' @param scenario_name (class string) name of the scenario assigned in the `params.yaml`.
#'
#' @return (class data.table) data.table containing time spend consecutively in each of the
#' simulation states for each site and the proportion for each tranmission route per site.
#' This contains the following information:
#' 1. `site_id` (integer) the site identification number.
#' 2. `totalDays_S1` (numeric) total time (days) in which the site has been in state 1
#'    (infected) through all of the simulations
#' 3. `proportion_state1` (numeric) proportion of time (days) in which the site has been
#'     in state 1 (infected) throughout the course of all the simulations
#' 4. `totalDays_S3` (numeric) total time (days) in which the site has been in state 3
#'    (Infected with secondary controls) through all of the simulations
#' 5. `proportion_state3` (numeric) proportion of time (days) in which the site has been
#'     in state 3 (Infected with secondary controls) throughout the course of all the simulations
#' 6. `totalDays_S0` (numeric) total time (days) in which the site has been in state 0 (uninfected)
#'     through all of the simulations
#' 7. `proportion_state0` (numeric) proportion of time (days) in which the site has been in state 0
#'     (Uninfected) throughout the course of all the simulations
#' 8. `totalDays_S2` (numeric) total time (days) in which the site has been in state 2 (Uninfected
#'     with secondary controls) through all of the simulations
#' 9. `proportion_state2` (numeric) proportion of time (days) in which the site has been in state 2
#'     (Uninfected with secondary controls) throughout the course of all the simulations
#' 8. `LFM`(numeric) number of transmissions due to LFMs for each site across all simulations
#' 9. `RB`(numeric) number of transmissions due to RBs for each site across all simulations
#' 10.`SDM`(numeric) number of transmissions due to SDM for each site across all simulations
#' 11.`DIM`(numeric) number of transmissions due to DIM for each site across all simulations
#' 12.`siteTotalTransmissions`(numeric) total number of transmissions for each site across all simulations
#' 13.`LFM_prop`(numeric) proportion of transmissions due to LFMs for each site across all simulations
#' 14.`RB_prop` (numeric) proportions of transmissions due to RB for each site across all simulations
#' 15.`SDM_prop`(numeric) proportions of transmissions due to SDM for each site across all simulations
#' 16.`DIM_prop`(numeric) proportions of transmissions due to DIM for each site across all simulations
#'
#' @export
#'
#' @import data.table
#' @importFrom here here
#' @importFrom arrow read_parquet write_parquet
#' @importFrom utils head

aggregateScenerioOutputs <- function(scenario_name){

  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  max_t <- sim_no <- sim_sum_time <- state <- t_total <- simNo <- sum_1_3 <- state0_sum <- NULL
  site_id <- proportion_state1 <- totalDays_S1 <-proportion_state1 <- totalDays_S0 <- NULL
  proportion_state0 <- totalDays_S2 <- proportion_state2 <- totalDays_S3 <- . <- NULL
  proportion_state3 <- trans_type <- siteTotalTransmissions <- LFM <- RB <- SDM <- DIM <- NULL

  # import the parquet file
  condensed_output <- arrow::read_parquet(here::here("outputs",
                                                     scenario_name,
                                                     "economics",
                                                     paste0(scenario_name, "-details-condensed.parquet")))
  # convert to data.table
  dt <- data.table::data.table(condensed_output)

  # epidemic duration
  full_results <- aquanet::loadResultsSummary(scenario_name)
  full_results <- data.table(full_results)
  data.table::setnames(full_results, old = "sim_no", new = "simNo")
  valid_results <- full_results[simNo != 0]
  no_days <- valid_results[, c("simNo", "t")][ # select simNo and t
    , by = simNo, .(max_t = max(t))][ # get maximum time per simNo
      , c("simNo", "max_t")] # select simNo and max_t

  # sum max_t for each simulation (for calculating proportions)
  #overall sum
  overall_scenerio_time <- no_days[, sum(max_t)]

  # If one digit= uninfected
  dt[, state := ifelse(nchar(state) == 1, sprintf("%02d", as.numeric(state)), state)]

  ###################################################################
  # State proportions
  ###################################################################

  # Proportion in state 1 (infected)

  # select only states that start with 1 (infected)
  state1<- dt[substr(as.character(state), 1, 1) %in% c("1")]

  # group by sites, sum t_total (sum of time spend in state)
  state1_sum <- state1[, .(totalDays_S1 = sum(t_total)), by = site_id]

  # Calculate proportion of time spent infected per site
  proportion_inf1 <- state1_sum[, proportion_state1 := totalDays_S1/overall_scenerio_time]

  #--------------------------------------------------------------------------
  # Proportion in state 3 (infected with secondary controls)

  # select only states that start with 3 (infected with secondary controls)
  state3_dt<- dt[substr(as.character(state), 1, 1) %in% c("3")]

  # group by sites, sum t_total (sum of time spend in state)
  state3_sum <- state3_dt[, .(totalDays_S3 = sum(t_total)), by = site_id]

  # Calculate proportion of time spent infected per site
  proportion_inf3 <- state3_sum[, proportion_state3 := totalDays_S3/overall_scenerio_time]


  #--------------------------------------------------------------------------
  # Proportion in state 2 (Uninfected with secondary controls)

  # select only states that start with 0 (Uninfected with secondary controls)
  state2_dt<- dt[substr(as.character(state), 1, 1) %in% c("2")]

  # group by sites, sum t_total (sum of time spend in state)
  state2_sum <- state2_dt[, .(totalDays_S2 = sum(t_total)), by = site_id]

  # Calculate proportion of time spent infected per site
  proportion_inf2 <- state2_sum[, proportion_state2 := totalDays_S2/overall_scenerio_time]

  #--------------------------------------------------------------------------
  # Proportion in state 0 (Uninfected)

  # Join states 1-3 into 1 data set

  ProportionsDt <- merge(proportion_inf1, proportion_inf3, by = "site_id", all = TRUE)
  ProportionsDt <- merge(ProportionsDt, proportion_inf2, by = "site_id", all = TRUE)

  # per site sum state 1-3
  ProportionsDt[,sum_1_3 := sum(totalDays_S1,totalDays_S2,totalDays_S3), by = site_id]

  # minus by over scenerio time
  ProportionsDt[,state0_sum := overall_scenerio_time-sum_1_3, by = site_id]

  # Calculate proportion of time spent infected per site
  ProportionsDt[,proportion_state0 := state0_sum/overall_scenerio_time, by = site_id]

  # drop column sum_1_3
  ProportionsDt[, sum_1_3:= NULL]

  ###################################################################
  # Transition proportions
  ###################################################################

  # Process the filtered data as in your original code
  infection_events_per_site <- dt[ ,
                                   .(LFM = sum(trans_type == 0),
                                     RB = sum(trans_type == 10),
                                     SDM = sum(trans_type == 14),
                                     DIM = sum(trans_type == 11)),
                                   by = site_id]

  # Get the total number of infections via each route
  infection_events_per_site[, siteTotalTransmissions := sum(LFM, RB, SDM, DIM), by = site_id]


  # calculate proportion of infections via each route- rounded to 2 d.p.
  infection_events_per_site[, `:=`(LFM_prop = LFM / siteTotalTransmissions,
                                   RB_prop = round(RB / siteTotalTransmissions, 2),
                                   SDM_prop = round(SDM / siteTotalTransmissions, 2),
                                   DIM_prop = round(DIM / siteTotalTransmissions,2))]

  ###################################################################
  # Join datasets
  ###################################################################

  # Join datasets- proportion states and transmission routes
  ProportionsDt <- merge(ProportionsDt, infection_events_per_site, by = "site_id", all = TRUE)

  # read in site type vector
  site_type <- fread(here::here("outputs",
                                scenario_name,
                                "categorisedSites.csv"))
  # join to sites_summary
  FinalDt <- data.table::merge.data.table(ProportionsDt,
                                          site_type,
                                          all.x = TRUE,
                                          by.x = "site_id",
                                          by.y = "Code")
  # write as csv
  write.csv(FinalDt, here::here("outputs",
                                scenario_name,
                                paste0(scenario_name,"SiteCondensedOutput.csv")))

  return(FinalDt)

}


