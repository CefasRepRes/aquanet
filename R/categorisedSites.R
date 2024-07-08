#' categorisedSites
#'
#' Sites from fisheries and farms are labelled if they fall into the following site type based on Tonnage/count produced per calander year; Table (Small (0-10T), Medium (>10-100T), Large (>100T)),
#' Restocker(Small (0-10T), Medium (>10-100T), Large (>100T)), Ongrower (Small(0-20T), Medium (>20-50T), Large (>50)), Hatchery (Small (0-1000K), Large (>1000K)) and Fishery ((Small (0-10T), Medium (>10-100T), Large (>100T)).
#' Using the Production dataset (including movement of dead fish)- which has been converted to change counts to tonnes (excluding hatcheries). A csv file is then outputted with the classifications for each site labelled using 'Y' and 'N'
#' based on if they meet the conditions for each site type.
#'
#' @param production_data (data.table) Production data as data.table (using function 'fread(dataset)') extracted from STARFISH, with columns 'Code', 'AuthorisationAndRegistrationStatusCode', 'CodeDescription', 'CodeDescription2', 'Year', 'AmountInTonnes', 'AmountInUnits', 'Units' included. included. If data is in the units 'K' for Table, Restocker, Ongrower or Fishery, the data has been converted into Tonnes using the average size of species recorder within the year (calculated using the unaggrated LFM data).
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

categorisedSites <- function(lfm_data, production_data, scenario_name){

  # define column names used with data.table syntax
  # NOTE: this satisfies "no visible binding for global variable" devtools::check()
  SmallRestocker <- MediumRestocker <- LargeRestocker <- SmallOngrower <- MediumOngrower <- LargeOngrower <- SmallTable <-
    MediumTable <- LargeTable <- SmallHatchery <- SmallHatch <- LargeHatchery <- LargeHatch <- SmallFishery <- MediumFishery <- LargeFishery <- Code <-
    AuthorisationAndRegistrationStatusCode <- Category <- CodeDescription2 <- CodeDescription <- AmountInUnits <- AmountInTonnes <- Year <- Units <- AnnualTonnes <- AnnualCount <- AmountInUnits <- NULL

  # rename 'Code' col from Production to match lfm_data
  colnames(production_data)[colnames(production_data) == "Code"] <- "Src_Code"

  # Label as 'Table' using production_data data if CodeDescription2 == 'Table'
  Table <- production_data[CodeDescription2 == 'Table']

  # Tonnes per year for site
  tonnes <- Table[, list(AnnualTonnes = sum(AmountIntonnes)), by = list(Src_Code, Year)]

  # Merge with original table dataset
  AnnualtonneTable <- merge(Table, tonnes, by = c("Src_Code", "Year"), all.x= TRUE)

  # If 0-10 label as small table
  AnnualtonneTable[, SmallTable := ifelse(AnnualTonnes >=0 & AnnualTonnes <=10, 'Y', 'N')]

  # If 10-100 label as medium table
  AnnualtonneTable[, MediumTable := ifelse(AnnualTonnes > 10 & AnnualTonnes <=100, 'Y', 'N')]

  # If 100-1000 label as large table
  AnnualtonneTable[, LargeTable := ifelse(AnnualTonnes > 100, 'Y', 'N')]


  # Classify Hatchery -----------------------------------------------------------------------------------------------

  # label as Hatchery using production_data if CodeDescription == 'egg' or eggs'
  Hatchery <- production_data[CodeDescription %in% c('EGG', 'EGGS')]

  # Counts (k) per year for site
  Count <- Hatchery[, list(AnnualCount = sum(AmountInUnits)), by = list(Src_Code, Year)]

  # Merge with original table dataset
  AnnualCountHatch <- merge(Hatchery, Count, by = c("Src_Code", "Year"), all.x= TRUE)

  # If 0-1000K label as small hatchery
  AnnualCountHatch[, SmallHatch := ifelse(AnnualCount >=0 & AnnualCount <=1000, 'Y', 'N')]

  # If >1000 label as large hatchery
  AnnualCountHatch[, LargeHatch := ifelse(AnnualCount > 1000, 'Y', 'N')]

  # Classify others using LFM data ---------------------------------------------

  ### Preprocessing
  lfmCategorised <- lfm_data %>% data.table()

  # Convert to date format
  lfmCategorised$MovementDate <- as.Date(lfmCategorised$MovementDate,
                                         format = "%d/%m/%Y")
  # Create a 'Year' column
  lfmCategorised[, Year := year(MovementDate)]

  # Convert TotalWeight_g to numeric, handle non-numeric conversion with na.rm
  lfmCategorised[, TotalWeight_g := as.numeric(TotalWeight_g)]

  # Tonnes per year for site
  AnnualTonnes <- lfmCategorised[, list(AnnualTonnes = sum(TotalWeight_g)/1000000), by = list(Src_Code, Year)]

  # Merge with lfm dataset
  Annualtonnelfm <- merge(lfmCategorised, AnnualTonnes, by = c("Src_Code", "Year"), all.x= TRUE)

  ## Classify source sites =====================================================

  # Restocker -----------------------------------------------------------

  # label as Restocker if 'Sport/Angling', 'Restocking, i.e. Salmonid stock enhancement' is contained within the PurposeOfMovement column
  Restocker <- Annualtonnelfm[PurposeOfMovement %in% c('Sport/Angling','Restocking, i.e. Salmonid stock enhancement')]

  # If 0-10 label as small Restocker
  Restocker[, SrcSmallRestocker := ifelse(AnnualTonnes >=0 & AnnualTonnes <=10, 'Y', 'N')]

  # If 10-100 label as medium Restocker
  Restocker[, SrcMediumRestocker := ifelse(AnnualTonnes > 10 & AnnualTonnes <=100, 'Y', 'N')]

  # If 100-1000 label as large Restocker
  Restocker[, SrcLargeRestocker := ifelse(AnnualTonnes > 100, 'Y', 'N')]

  # Ongrower --------------------------------------------------------------

  # Label as Ongrower if 'Ongrowing' is contained within the PurposeOfMovement column
  Ongrower <- Annualtonnelfm[PurposeOfMovement =='Ongrowing']

  # If 0-10 label as small Ongrower
  Ongrower[, SrcSmallOngrower := ifelse(AnnualTonnes >=0 & AnnualTonnes <=20, 'Y', 'N')]

  # If 10-100 label as medium Ongrower
  Ongrower[, SrcMediumOngrower := ifelse(AnnualTonnes > 20 & AnnualTonnes <=50, 'Y', 'N')]

  # If 100-1000 label as large Ongrower
  Ongrower[, SrcLargeOngrower := ifelse(AnnualTonnes > 50, 'Y', 'N')]

  # Fishery --------------------------------------------------------------

  # Label as Fishery if Scr_AutRegStatus == 'REGFISH'
  SrcFishery <- Annualtonnelfm[Scr_AutRegStatus == 'REGFISH']

  # If 0-10 label as small Fishery
  SrcFishery[, SrcSmallFishery := ifelse(AnnualTonnes >=0 & AnnualTonnes <=10, 'Y', 'N')]

  # If 10-100 label as medium Fishery
  SrcFishery[, SrcMediumFishery := ifelse(AnnualTonnes > 10 & AnnualTonnes <=100, 'Y', 'N')]

  # If 100-1000 label as large Fishery
  SrcFishery[, SrcLargeFishery := ifelse(AnnualTonnes > 100, 'Y', 'N')]


  # Classify destination sites ================================================

  # label as Restocker if 'Sport/Angling', 'Restocking, i.e. Salmonid stock enhancement' is contained within the PurposeOfMovement column

  # If 0-10 label as small Restocker
  Restocker[, DestSmallRestocker := ifelse(AnnualTonnes >=0 & AnnualTonnes <=10, 'Y', 'N')]

  # If 10-100 label as medium Restocker
  Restocker[, DestMediumRestocker := ifelse(AnnualTonnes > 10 & AnnualTonnes <=100, 'Y', 'N')]

  # If 100-1000 label as large Restocker
  Restocker[, DestLargeRestocker := ifelse(AnnualTonnes > 100, 'Y', 'N')]

  # Ongrower --------------------------------------------------------------

  # Label as Ongrower if 'Ongrowing' is contained within the PurposeOfMovement column
  Ongrower <- Annualtonnelfm[PurposeOfMovement =='Ongrowing']

  # If 0-10 label as small Ongrower
  Ongrower[, DestSmallOngrower := ifelse(AnnualTonnes >=0 & AnnualTonnes <=20, 'Y', 'N')]

  # If 10-100 label as medium Ongrower
  Ongrower[, DestMediumOngrower := ifelse(AnnualTonnes > 20 & AnnualTonnes <=50, 'Y', 'N')]

  # If 100-1000 label as large Ongrower
  Ongrower[, DestLargeOngrower := ifelse(AnnualTonnes > 50, 'Y', 'N')]

  # Fishery --------------------------------------------------------------

  # Label as Fishery if Scr_AutRegStatus == 'REGFISH'
  DestFishery <- Annualtonnelfm[Dest_AutRegStatus == 'REGFISH']

  # If 0-10 label as small Fishery
  DestFishery[, DestSmallFishery := ifelse(AnnualTonnes >=0 & AnnualTonnes <=10, 'Y', 'N')]

  # If 10-100 label as medium Fishery
  DestFishery[, DestMediumFishery := ifelse(AnnualTonnes > 10 & AnnualTonnes <=100, 'Y', 'N')]

  # If 100-1000 label as large Fishery
  DestFishery[, DestLargeFishery := ifelse(AnnualTonnes > 100, 'Y', 'N')]

  ## Melt source and destination sites together ==================================

  # get unique sites
  sites <- data.frame(Code = c(Annualtonnelfm$Src_Code, Annualtonnelfm$Dest_Code, AnnualtonneTable$Src_Code, AnnualCountHatch$Src_Code),
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
    prodTable <- AnnualtonneTable[Src_Code == sites$Code[i]]
    prodHatch <- AnnualCountHatch[Src_Code == sites$Code[i]]
    SrcOngrow <- Ongrower[Src_Code == sites$Code[i]]
    SrcRest <- Restocker[Src_Code == sites$Code[i]]
    SrcFish <- SrcFishery[Src_Code == sites$Code[i]]
    DestOngrow <- Ongrower[Dest_Code == sites$Code[i]]
    DestRest <- Restocker[Dest_Code == sites$Code[i]]
    DestFish <- DestFishery[Dest_Code == sites$Code[i]]
    #Source ----------------------------------------
    #Small Hatchery
    if("Y" %in% prodHatch$SmallHatch){
      sites$SmallHatchery[i] <- "Y"
    }
    # Large Hatchery
    if("Y" %in% prodHatch$LargeHatch){
      sites$LargeHatchery[i] <- "Y"
    }
    # Small Ongrower
    if("Y" %in% SrcOngrow$SrcSmallOngrower){
      sites$SmallOngrower[i] <- "Y"
    }
    # Med Ongrower
    if("Y" %in% SrcOngrow$SrcMediumOngrower){
      sites$MediumOngrower[i] <- "Y"
    }
    # Large Ongrower
    if("Y" %in% SrcOngrow$SrcLargeOngrower){
      sites$LargeOngrower[i] <- "Y"
    }
    # Small Restocker
    if("Y" %in% SrcRest$SrcSmallRestocker){
      sites$SmallRestocker[i] <- "Y"
    }
    # Medium Restocker
    if("Y" %in% SrcRest$SrcMediumRestocker){
      sites$MediumRestocker[i] <- "Y"
    }
    # Large Restocker
    if("Y" %in% SrcRest$SrcLargeRestocker){
      sites$LargeRestocker[i] <- "Y"
    }
    # Small Table
    if("Y" %in% prodTable$SmallTable){
      sites$SmallTable[i] <- "Y"
    }
    # Medium Table
    if("Y" %in% prodTable$MediumTable){
      sites$MediumTable[i] <- "Y"
    }
    # Large Table
    if("Y" %in% prodTable$LargeTable){
      sites$LargeTable[i] <- "Y"
    }
    # Small Fishery
    if("Y" %in% SrcFish$SrcSmallFishery){
      sites$SmallFishery[i] <- "Y"
    }
    # Medium Fishery
    if("Y" %in% SrcFish$SrcMediumFishery){
      sites$MediumFishery[i] <- "Y"
    }
    # Large Fishery
    if("Y" %in% SrcFish$SrcLargeFishery){
      sites$LargeFishery[i] <- "Y"
    }
    #Dest ----------------------------------------

    # Small Ongrower
    if("Y" %in% DestOngrow$DestSmallOngrower){
      sites$SmallOngrower[i] <- "Y"
    }
    # Med Ongrower
    if("Y" %in% DestOngrow$DestMediumOngrower){
      sites$MediumOngrower[i] <- "Y"
    }
    # Large Ongrower
    if("Y" %in% DestOngrow$DestLargeOngrower){
      sites$LargeOngrower[i] <- "Y"
    }

    # Small Restocker
    if("Y" %in% DestRest$DestSmallRestocker){
      sites$SmallRestocker[i] <- "Y"
    }
    # Medium Restocker
    if("Y" %in% DestRest$DestMediumRestocker){
      sites$MediumRestocker[i] <- "Y"
    }
    # Large Restocker
    if("Y" %in% DestRest$DestLargeRestocker){
      sites$LargeRestocker[i] <- "Y"
    }
    # Small Fishery
    if("Y" %in% DestFish$SmallFishery){
      sites$SmallFishery[i] <- "Y"
    }
    # Medium Fishery
    if("Y" %in% DestFish$MediumFishery){
      sites$MediumFishery[i] <- "Y"
    }
    # Large Fishery
    if("Y" %in% DestFish$LargeFishery){
      sites$LargeFishery[i] <- "Y"
    }
  }

  # Classify as farm or fishery -----------------------------------------------------------------------------------------------

  # classify as either farm or fishery using the Scr_AutRegStatus column
  lfmCategorised[, Scr_Category := ifelse(grepl("FARM", lfmCategorised$Scr_AutRegStatus, ignore.case = TRUE), "Farm",
                                          ifelse(Scr_AutRegStatus == 'REGFISH', 'Fishery', 'Other'))]
  lfmCategorised[, Dest_Category := ifelse(grepl("FARM", lfmCategorised$Dest_AutRegStatus, ignore.case = TRUE), "Farm",
                                           ifelse(Dest_AutRegStatus == 'REGFISH', 'Fishery', 'Other'))]

  # melt to long format with source and destination sites
  lfmCategorised_long <- data.frame(Code = c(lfmCategorised$Src_Code,
                                             lfmCategorised$Dest_Code),
                                    Category = c(lfmCategorised$Scr_Category,
                                                 lfmCategorised$Dest_Category)) %>% unique()

  # Clean up final dataset -----------------------------------------------------------------------------------------------

  # combine with sites
  # combine with sites
  all_data <- merge(sites,
                    lfmCategorised_long,
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

