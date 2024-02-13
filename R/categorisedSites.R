#' categorisedSites
#'
#' Sites from fisheries and farms are labeled if they fall into the following site type; Table, Restocker, Ongrower, Hatchery and Fishery.
#' Using both the LFM and Production datasets. A csv file is then outputed with the classifications for each site labelled using 'Y' and 'N'
#' based on if they meet the conditions for each site type.
#'
#' @param lfmData (data.table) Live Fish Movement data extracted from STARFISH database with columns 'Src_Code', 'Scr_AutRegStatus', 'PurposeOfMovement' included.
#' @param ProductionData (data.table) Production data as data.table (using function 'fread(dataset)') extracted from STARFISH, with columns 'Code', 'AuthorisationAndRegistrationStatusCode', 'CodeDescription2' included.
#' @param scenario_name (character) Name of the scenario being run. Set in the params .yml file.
#'
#' @return a csv file with site categorised by type; 'Farm/fishery', 'Table', 'Restocker', 'Ongrower', 'Hatchery', 'Fishery'.
#' A site can fall into multiple categories.
#' @import data.table
#' @import here
#' @importFrom utils write.csv
#' @export
#'
categorisedSites <- function(lfmData, ProductionData, scenario_name){
  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  Category <- CodeDescription2 <- DevCode <- Fishery <- Hatchery <- Ongrower <-
    PurposeOfMovement <- Restocker <- Scr_AutRegStatus <- Table <- Tablelfm <- NULL

  # Classify Table from ProductionData data

  # Rename 'Code' col from Production to match lfmData
  colnames(ProductionData)[colnames(ProductionData) == "Code"] <- "Src_Code"

  # Label as 'Table' using ProductionData data if CodeDescription2 == 'Table'
  ProductionData[, Table := ifelse(CodeDescription2 == 'Table', 'Y', 'N')]

  # Remove unwanted cols
  ProductionDataFarm <- ProductionData[, c("Src_Code", "Table"), with = FALSE]


  # Categories using LFM data

  # Label as Ongrower if 'Ongrowing', 'Broodstock' is contained within the PurposeOfMovement column
  lfmCategorised <- lfmData[, Ongrower := ifelse(PurposeOfMovement %in% c('Ongrowing', 'Broodstock'), 'Y', 'N')]

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
  lfmCategorised <- lfmData[, Tablelfm := ifelse(PurposeOfMovement == 'Slaughter', 'Y', 'N')]

  # Remove duplicates from LFM categorised dataset (as multiple movements per farm)
  farmType <- unique(lfmCategorised, by = "Src_Code")



  # Merge ProductionData and lfmData
  ProductionDataLFM <- merge(farmType, ProductionDataFarm, by = 'Src_Code', all.x = TRUE)
  head(ProductionDataLFM)

  # Combine LFM and ProductionData 'Table' columns- filling in from lfm if production 'Table' column equals 'N' or 'NA'
  test <- ProductionDataLFM[, Table := ifelse(is.na(Table) | Table == "N", Tablelfm, Table)]
  head(test)

  #remove unwanted cols
  categorisedSites <- ProductionDataLFM[, c("Src_Code", "Scr_AutRegStatus", "Category", "Table", "Ongrower",  "Restocker", "Hatchery", "Fishery"), with = FALSE]
  print(head(categorisedSites))

  # Save final dataset as csv
  utils::write.csv(categorisedSites, here::here("outputs",
                                         scenario_name,
                                         "categorisedSites.csv"))

}
