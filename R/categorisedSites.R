#' categorisedSites
#'
#' Sites from fisheries and farms are labelled if they fall into the following site type based on Tonnage/count produced per calander year; Table (Small (0-10T), Medium (>10-100T), Large (>100T)),
#' Restocker(Small (0-10T), Medium (>10-100T), Large (>100T)), Ongrower (Small(0-20T), Medium (>20-50T), Large (>50)), Hatchery (Small (0-1000K), Large (>1000K)) and Fishery ((Small (0-10T), Medium (>10-100T), Large (>100T)).
#' Using the Production dataset (including movement of dead fish). A csv file is then outputted with the classifications for each site labelled using 'Y' and 'N'
#' based on if they meet the conditions for each site type.
#'
#' @param production_data (data.table) Production data as data.table (using function 'fread(dataset)') extracted from STARFISH, with columns 'Code', 'AuthorisationAndRegistrationStatusCode', 'CodeDescription', 'CodeDescription2', 'Year', 'AmountInUnits', 'Units' included. included. If data is in the units 'K' for Table, Restocker, Ongrower or Fishery, the data has been converted into Tonnes using the average size of species recorder within the year (calculated using the unaggrated LFM data).
#' @param scenario_name (character) Name of the scenario being run. Set in the params .yml file.
#'
#' @return a csv file with site categorised by type; 'Farm/fishery', 'SmallTable', 'MediumTable, 'LargeTable, 'SmallRestocker',
#' 'MediumRestocker', 'LargeRestocker', 'SmallOngrower', 'MediumOngrower', 'LargeOngrower', 'SmallHatchery', 'LargeHatchery', 'SmallFishery', 'MediumFishery', 'LargeFishery'
#' A site can fall into multiple categories.
#' @import data.table
#' @import here
#' @import magrittr
#' @importFrom utils write.csv
#' @export

categorisedSites <- function(production_data, scenario_name){

  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  SmallRestocker <- MediumRestocker <- LargeRestocker <- SmallOngrower <- MediumOngrower <- LargeOngrower <- SmallTable <-
    MediumTable <- LargeTable <- SmallHatchery <- SmallHatch <- LargeHatchery <- LargeHatch <- SmallFishery <- MediumFishery <- LargeFishery <- Code <-
    AuthorisationAndRegistrationStatusCode <- Category <- CodeDescription2 <- CodeDescription <- AmountInUnits <- Year <- Units <- AnnualTonnes <- AnnualCount <- AmountInUnits <- NULL

  # Label as 'Table' using production_data data if CodeDescription2 == 'Table'
  Table <- production_data[CodeDescription2 == 'Table']

  # Tonnes per year for site
  tonnes <- Table[, list(AnnualTonnes = sum(AmountInUnits)), by = list(Code, Year)]

  # Merge with original table dataset
  AnnualtonneTable <- merge(Table, tonnes, by = c("Code", "Year"), all.x= TRUE)

  # If 0-10 label as small table
  AnnualtonneTable[, SmallTable := ifelse(AnnualTonnes >=0 & AnnualTonnes <=10, 'Y', 'N')]

  # If 10-100 label as medium table
  AnnualtonneTable[, MediumTable := ifelse(AnnualTonnes > 10 & AnnualTonnes <=100, 'Y', 'N')]

  # If 100-1000 label as large table
  AnnualtonneTable[, LargeTable := ifelse(AnnualTonnes > 100, 'Y', 'N')]


  # Classify Hatchery -----------------------------------------------------------------------------------------------

  # label as Hatchery using production_data if CodeDescription == 'egg' or eggs'
  Hatchery <- production_data[CodeDescription %in% c('EGG', 'EGGS')]
  head(Hatchery)

  # Counts (k) per year for site
  Count <- Hatchery[, list(AnnualCount = sum(AmountInUnits)), by = list(Code, Year)]

  # Merge with original table dataset
  AnnualCountHatch <- merge(Hatchery, Count, by = c("Code", "Year"), all.x= TRUE)

  # If 0-1000K label as small hatchery
  AnnualCountHatch[, SmallHatch := ifelse(AnnualCount >=0 & AnnualCount <=1000, 'Y', 'N')]

  # If >1000 label as large hatchery
  AnnualCountHatch[, LargeHatch := ifelse(AnnualCount > 1000, 'Y', 'N')]


  # Classify Restocker -----------------------------------------------------------------------------------------------

  # Label as 'Restocker' using production_data data if CodeDescription2 == 'Restocking' or 'Fishing/Sport'
  Restocker <- production_data[CodeDescription2 %in% c('Restocking','Fishing/Sport')]

  # Tonnes per year for site
  tonnes <- Restocker[, list(AnnualTonnes = sum(AmountInUnits)), by = list(Code, Year)]

  # Merge with original Restocker dataset
  AnnualtonneRestocker <- merge(Restocker, tonnes, by = c("Code", "Year"), all.x= TRUE)

  # If 0-10 label as small Restocker
  AnnualtonneRestocker[, SmallRestocker := ifelse(AnnualTonnes >=0 & AnnualTonnes <=10, 'Y', 'N')]

  # If 10-100 label as medium Restocker
  AnnualtonneRestocker[, MediumRestocker := ifelse(AnnualTonnes > 10 & AnnualTonnes <=100, 'Y', 'N')]

  # If 100-1000 label as large Restocker
  AnnualtonneRestocker[, LargeRestocker := ifelse(AnnualTonnes > 100, 'Y', 'N')]


  # Classify Ongrower -----------------------------------------------------------------------------------------------

  # Label as 'Ongrower' using production_data data if CodeDescription2 == 'Restocking' or 'Fishing/Sport'
  Ongrower <- production_data[CodeDescription2 == 'Ongrowing']

  # Tonnes per year for site
  tonnes <- Ongrower[, list(AnnualTonnes = sum(AmountInUnits)), by = list(Code, Year)]

  # Merge with original Ongrower dataset
  AnnualtonneOngrower <- merge(Ongrower, tonnes, by = c("Code", "Year"), all.x= TRUE)

  # If 0-10 label as small Ongrower
  AnnualtonneOngrower[, SmallOngrower := ifelse(AnnualTonnes >=0 & AnnualTonnes <=20, 'Y', 'N')]

  # If 10-100 label as medium Ongrower
  AnnualtonneOngrower[, MediumOngrower := ifelse(AnnualTonnes > 20 & AnnualTonnes <=50, 'Y', 'N')]

  # If 100-1000 label as large Ongrower
  AnnualtonneOngrower[, LargeOngrower := ifelse(AnnualTonnes > 50, 'Y', 'N')]


  # Classify Fishery ---------------------------------------------------

  # Label as fishery if AutRegStatus is equal to 'REGFISH'
  # Fishery

  # Label as 'Fishery' using production_data data if AuthorisationAndRegistrationStatusCode == 'REGFISH'
  Fishery <- production_data[AuthorisationAndRegistrationStatusCode == 'REGFISH']

  # Tonnes per year for site
  tonnes <- Fishery[, list(AnnualTonnes = sum(AmountInUnits)), by = list(Code, Year)]

  # Merge with original Fishery dataset
  AnnualtonneFishery <- merge(Fishery, tonnes, by = c("Code", "Year"), all.x= TRUE)

  # If 0-10 label as small Fishery
  AnnualtonneFishery[, SmallFishery := ifelse(AnnualTonnes >=0 & AnnualTonnes <=10, 'Y', 'N')]

  # If 10-100 label as medium Fishery
  AnnualtonneFishery[, MediumFishery := ifelse(AnnualTonnes > 10 & AnnualTonnes <=100, 'Y', 'N')]

  # If 100-1000 label as large Fishery
  AnnualtonneFishery[, LargeFishery := ifelse(AnnualTonnes > 100, 'Y', 'N')]



  # Melt all datasets -----------------------------------------------------------------------------------------------

  # Get unique sites
  sites <- data.frame(Code = c(production_data$Code),
                      SmallHatchery = NA,
                      LargeHatchery = NA,
                      SmallOngrower = NA,
                      MediumOngrower = NA,
                      LargeOngrower = NA,
                      SmallRestocker = NA,
                      MediumRestocker = NA,
                      LargeRestocker = NA,
                      SmallTable = NA,
                      MediumTable = NA,
                      LargeTable = NA,
                      SmallFishery = NA,
                      MediumFishery = NA,
                      LargeFishery = NA) %>% unique()

  # loop to get types
  for(i in 1:nrow(sites)){
    Hatch <- AnnualCountHatch[Code == sites$Code[i]]
    Ongrow <- AnnualtonneOngrower[Code == sites$Code[i]]
    Rest <- AnnualtonneRestocker[Code == sites$Code[i]]
    Table <- AnnualtonneTable[Code == sites$Code[i]]
    Fish <- AnnualtonneFishery[Code == sites$Code[i]]
    #Small Hatchery
    if("Y" %in% Hatch$SmallHatch){
      sites$SmallHatchery[i] <- "Y"
    }
    # Large Hatchery
    if("Y" %in% Hatch$LargeHatch){
      sites$LargeHatchery[i] <- "Y"
    }
    # Small Ongrower
    if("Y" %in% Ongrow$SmallOngrower){
      sites$SmallOngrower[i] <- "Y"
    }
    # Med Ongrower
    if("Y" %in% Ongrow$MediumOngrower){
      sites$MediumOngrower[i] <- "Y"
    }
    # Large Ongrower
    if("Y" %in% Ongrow$LargeOngrower){
      sites$LargeOngrower[i] <- "Y"
    }

    # Small Restocker
    if("Y" %in% Rest$SmallRestocker){
      sites$SmallRestocker[i] <- "Y"
    }
    # Medium Restocker
    if("Y" %in% Rest$MediumRestocker){
      sites$MediumRestocker[i] <- "Y"
    }
    # Large Restocker
    if("Y" %in% Rest$LargeRestocker){
      sites$LargeRestocker[i] <- "Y"
    }
    # Small Table
    if("Y" %in% Table$SmallTable){
      sites$SmallTable[i] <- "Y"
    }
    # Medium Table
    if("Y" %in% Table$MediumTable){
      sites$MediumTable[i] <- "Y"
    }
    # Large Table
    if("Y" %in% Table$LargeTable){
      sites$LargeTable[i] <- "Y"
    }
    # Small Fishery
    if("Y" %in% Fish$SmallFishery){
      sites$SmallFishery[i] <- "Y"
    }
    # Medium Fishery
    if("Y" %in% Fish$MediumFishery){
      sites$MediumFishery[i] <- "Y"
    }
    # Large Fishery
    if("Y" %in% Fish$LargeFishery){
      sites$LargeFishery[i] <- "Y"
    }
  }

  # Classify as farm or fishery -----------------------------------------------------------------------------------------------

  # classify as either farm or fishery using the Scr_AutRegStatus column
  production_data[, Category := ifelse(grepl("Farm", production_data$AuthorisationAndRegistrationStatusCode, ignore.case = TRUE), "FARM",
                                       ifelse(AuthorisationAndRegistrationStatusCode == 'REGFISH', 'Fishery', 'Other'))]

  # melt to long format with source and destination sites
  Production_category <- data.frame(Code = production_data$Code,
                                    Category = production_data$Category) %>% unique()

  # Clean up final dataset -----------------------------------------------------------------------------------------------

  # combine with sites
  all_data <- merge(sites,
                    Production_category,
                    by = "Code",
                    all.x = TRUE)

  # replace NA with "N"
  all_data[is.na(all_data)] <- "N"

  # print head
  print(head(all_data))

  # Save final dataset as csv
  utils::write.csv(all_data, here::here("outputs",
                                        scenario_name,
                                        "categorisedSites.csv"),
                   row.names = FALSE)
  return(all_data)

}

