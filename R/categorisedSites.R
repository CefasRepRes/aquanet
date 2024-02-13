#' categorisedSites
#'
#' Sites from fisheries and farms are labelled if they fall into the following site type; Table, Restocker, Ongrower, Hatchery and Fishery.
#' Using both the LFM and Production datasets. A csv file is then outputted with the classifications for each site labelled using 'Y' and 'N'
#' based on if they meet the conditions for each site type.
#'
#' @param lfm_data (data.table) Live Fish Movement data extracted from STARFISH database with columns 'Src_Code', 'Scr_AutRegStatus', 'PurposeOfMovement' included.
#' @param production_data (data.table) Production data as data.table (using function 'fread(dataset)') extracted from STARFISH, with columns 'Code', 'AuthorisationAndRegistrationStatusCode', 'CodeDescription2' included.
#' @param scenario_name (character) Name of the scenario being run. Set in the params .yml file.
#'
#' @return a csv file with site categorised by type; 'Farm/fishery', 'Table', 'Restocker', 'Ongrower', 'Hatchery', 'Fishery'.
#' A site can fall into multiple categories.
#' @import data.table
#' @import here
#' @importFrom utils write.csv
#' @export
#'
categorisedSites <- function(lfm_data, production_data, scenario_name){
  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  Category <- CodeDescription2 <- DevCode <- Fishery <- Hatchery <- Ongrower <-
    PurposeOfMovement <- Restocker <- Scr_AutRegStatus <- Table <- Tablelfm <- NULL

  # Classify Table from ProductionData data

  # Rename 'Code' col from Production to match lfm_data
  colnames(production_data)[colnames(production_data) == "Code"] <- "Src_Code"

  # Label as 'Table' using production_data data if CodeDescription2 == 'Table'
  production_data[, Table := ifelse(CodeDescription2 == 'Table', 'Y', 'N')]

  # Remove unwanted cols
  production_dataFarm <- production_data[, c("Src_Code", "Table"), with = FALSE]


  # Categories using LFM data

  # Label as Ongrower if 'Ongrowing', 'Broodstock' is contained within the PurposeOfMovement column
  lfmCategorised <- lfm_data[, Ongrower := ifelse(PurposeOfMovement %in% c('Ongrowing', 'Broodstock'), 'Y', 'N')]

  # Label as Restocker if 'Sport/Angling', 'Restocking, i.e. Salmonid stock enhancement' is contained within the PurposeOfMovement column
  lfmCategorised[, Restocker := ifelse(PurposeOfMovement %in% c('Sport/Angling', 'Restocking, i.e. Salmonid stock enhancement'), 'Y', 'N')]

  # Label as hatchery if DevCode = 'EGG'
  lfmCategorised[, Hatchery := ifelse(DevCode == 'EGG', 'Y', 'N') ]

  # Label as fishery if Scr_AutRegStatus is equal to 'REGFISH'
  lfmCategorised[, Fishery := ifelse(Scr_AutRegStatus == 'REGFISH', 'Y', 'N') ]

  # Classify as either farm or fishery using the Scr_AutRegStatus column
  lfmCategorised[, Category := ifelse(grepl("FARM", lfmCategorised$Scr_AutRegStatus, ignore.case = TRUE), "Farm",
                                      ifelse(Scr_AutRegStatus == 'REGFISH', 'Fishery', 'Other'))]
  # Label as Table if PurposeOfMovement is equal to 'Slaughter'
  lfmCategorised <- lfm_data[, Tablelfm := ifelse(PurposeOfMovement == 'Slaughter', 'Y', 'N')]

  # Remove duplicates from LFM categorised dataset (as multiple movements per farm)
  farmType <- unique(lfmCategorised, by = "Src_Code")



  # Merge production_data and lfm_data
  production_dataLFM <- merge(farmType, production_dataFarm, by = 'Src_Code', all.x = TRUE)
  head(production_dataLFM)

  # Combine LFM and production_data 'Table' columns- filling in from lfm if production 'Table' column equals 'N' or 'NA'
  test <- production_dataLFM[, Table := ifelse(is.na(Table) | Table == "N", Tablelfm, Table)]
  head(test)

  # remove unwanted cols
  categorisedSites <- production_dataLFM[, c("Src_Code", "Scr_AutRegStatus", "Category", "Table", "Ongrower",  "Restocker", "Hatchery", "Fishery"), with = FALSE]
  print(head(categorisedSites))

  # Save final dataset as csv
  utils::write.csv(categorisedSites, here::here("outputs",
                                         scenario_name,
                                         "categorisedSites.csv"))

}
