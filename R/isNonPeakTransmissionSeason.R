#' isNonPeakTransmissionSeason
#'
#' This function determines whether the current simulation time is within the non-peak transmission
#' season where allowed disease transmission routes are different (see details).
#'
#' Function checks that the `period` value entered is equal to either "180", "90" or "0". Depending
#' on the value of `period` supplied the equation to determine seasonality is then selected.
#'
#' For example if `period = "180` the non peak transmission season lasts for 180 days, meaning there
#' are 2 seasons within the 360 day simulation year and the second season within the year is the
#' non peak transmission season.
#'
#' If `period = "90"` the non peak transmission season lasts for 90 days, meaning there are 4
#' seasons within the 360 day simulation year and the fourth season within the year is the non peak
#' transmission season.
#'
#' #' If `period = "28"` the non peak transmission season lasts for 28 days, meaning there are 12
#' seasons within the 360 day simulation year and the 12 season within the year is the non peak
#' transmission season.
#'
#' If `period == "0"` the non peak transmission season is 0 days long and never occurs.
#'
#' @param t (class numeric) time in simulation.
#'
#' @param period (class string) string containing the number of days that the non peak transmission
#' period lasts for. The function currently accepts "180" indicating two seasons of 180 days length
#' where the non peak season occurs second or "90" indicating four seasons of 90 days length where
#' the non peak season occurs in the fourth season of the year.
#'
#' @return (class logical) TRUE or FALSE statement of whether it is currently non peak transmission
#' season in the current simulation time `t`.
#'
#' @export
isNonPeakTransmissionSeason <- function(t,
                                        period = c("180", "90", "28", "0")) {

  # check period matches accepted values
  period <- match.arg(arg = period)

  # depending on value of period switch seasonality statement
  switch(
    EXPR = period,
    "180" = ((t %/% 180) %% 2) == 1,
    "90" = ((t %/% 90) %% 4) == 3,
    "28" = ((t %/% 28) %% 12) == 11,
    "0" = FALSE
  )
}
