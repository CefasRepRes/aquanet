#' categorisedSites
#'
#' Sites from fisheries and farms are labelled if they fall into the following site type based on Tonnage/count produced per calander year; Table (Small (0-10T), Medium (>10-100T), Large (>100T)),
#' Restocker(Small (0-10T), Medium (>10-100T), Large (>100T)), Ongrower (Small(0-20T), Medium (>20-50T), Large (>50)), Hatchery (Small (0-1000K), Large (>1000K)) and Fishery ((Small (0-10T), Medium (>10-100T), Large (>100T)).
#' Using the Production dataset (including movement of dead fish)- which has been converted to change counts to tonnes (excluding hatcheries). A csv file is then outputted with the classifications for each site labelled using 'Y' and 'N'
#' based on if they meet the conditions for each site type.
#'
#' @param production_data (data.table) Production data as data.table (using function 'fread(dataset)') extracted from STARFISH, with columns 'Code', 'AuthorisationAndRegistrationStatusCode', 'CodeDescription', 'CodeDescription2', 'Year', 'AmountInTonnes', 'AmountInUnits', 'Units' included. included. If data is in the units 'K' for Table, Restocker, Ongrower or Fishery, the data has been converted into Tonnes using the average size of species recorder within the year (calculated using the unaggrated LFM data).
#' @param lfm_data (data.table) Live fish Movement data as data.table (using function 'fread(dataset)') extracted from STARFISH, with columns 'MovementDate', 'Src_code', 'Dest_Code', 'DevCode', 'Scr_AutRegStatus', 'Dest_AutRegStatus', 'PurposeOfMovement', 'TotalWeight_g' included.
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
    AuthorisationAndRegistrationStatusCode <- Category <- CodeDescription2 <- CodeDescription <- AmountInUnits <- AmountInTonnes <- Year <- Units <- . <-
    AnnualTonnes <- AnnualCount <- AmountInUnits <- AmountIntonnes <- Src_Code <- MovementDate <- TotalWeight_g <- PurposeOfMovement <- SmallRestocker <-
    MediumRestocker <- LargeRestocker <-SmallOngrower <- MediumOngrower <- LargeOngrower <- Scr_AutRegStatus <- SmallFishery <- MeanAnnualTones <- MeanAnnualCount <-
    MediumFishery <- LargeFishery <-  Dest_Code <- Scr_Category <- Dest_Category <- Dest_AutRegStatus <- DevCode <- sum_averageAnnualTonnes <- Src_Category <- prod_Category <-
    SrcFarm <-  DestFarm <-  SrcFishery<- DestFishery  <- Restockerdest <- Restockersrc <- Ongrower <- Restocker <- Ongrowerdest <- Ongrowersrc <- notSrcFarm <- notDestFarm <- NULL

    # We have only used production data for table farms so no need to split into farm/fishery (as only AUTFARM extracted).
    #If this changes in the future- add to this function

    colnames(production_data)[colnames(production_data) == "Code"] <- "Src_Code"

    # Label as 'Table' using production_data data if CodeDescription2 == 'Table'
    Table <- production_data[CodeDescription2 == 'Table']

    # Tonnes per year for site
    tonnes <- Table[, .(AnnualTonnes = sum(AmountIntonnes)), by = .(Src_Code, Year)]

    # Calculate annual mean per sites
    meanAnnualTonneTable <- tonnes[, .(MeanAnnualTones = mean(AnnualTonnes)), by = Src_Code]

    # Merge with original table dataset
    AnnualtonneTable <- merge(Table, meanAnnualTonneTable, by = c("Src_Code"), all.x= TRUE)

    # If 0-10 label as small table
    AnnualtonneTable[, SmallTable := ifelse(MeanAnnualTones >=0 & MeanAnnualTones <=10, 'Y', 'N')]

    # If 10-100 label as medium table
    AnnualtonneTable[, MediumTable := ifelse(MeanAnnualTones > 10 & MeanAnnualTones <=100, 'Y', 'N')]

    # If over 100 label as large table
    AnnualtonneTable[, LargeTable := ifelse(MeanAnnualTones > 100, 'Y', 'N')]


    # Classify Hatchery -----------------------------------------------------------------------------------------------


    # label as Hatchery using production_data by CodeDescription2

    Hatchery <-production_data[grepl("\\bEggs moved ON\\b", CodeDescription2) |
                                 grepl("\\bEggs moved OFF\\b", CodeDescription2)|
                                 grepl("\\bOwn eggs laid down\\b", CodeDescription2),]

    # Counts (k) per year for site
    Count <- Hatchery[, .(AnnualCount = sum(AmountInUnits)), by = .(Src_Code, Year)]

    # Caluculate annual mean per sites
    meanAnnualCountHatch <- Count [, .(MeanAnnualCount = mean(AnnualCount)), by = Src_Code]

    # merge datasets
    MeanAnnualCountHatch <- merge(Hatchery, meanAnnualCountHatch, by = c("Src_Code"), all.x= TRUE)

    # If 0-1000K label as small hatchery
    meanAnnualCountHatch[, SmallHatch := ifelse(MeanAnnualCount >=0 & MeanAnnualCount<=1000, 'Y', 'N')]

    # If >1000 label as large hatchery
    meanAnnualCountHatch[, LargeHatch := ifelse(MeanAnnualCount > 1000, 'Y', 'N')]

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


    ## Classify source & Dest sites =====================================================

    # If AUTFARM in included in Scr_AutRegStatus = farm (even is regfish is also included)
    #ensures that only the whole word is matched, \\b is a word boundary, ensuring we match "in" as a separate word rather than a substring
    SrcFarm <- lfmCategorised[grepl("\\bAUTFARM\\b", Scr_AutRegStatus)]
    DestFarm <- lfmCategorised[grepl("\\bAUTFARM\\b", Dest_AutRegStatus)]

    SrcFarm <- SrcFarm[DevCode != 'EGG']
    DestFarm <- DestFarm[DevCode != 'EGG']

    # Restocker Source & Dest -----------------------------------------------------------

    # label as Restocker if 'Sport/Angling', 'Restocking, i.e. Salmonid stock enhancement' is contained within the PurposeOfMovement column
    Restockersrc <- SrcFarm[grepl("\\bSport/Angling\\b", PurposeOfMovement) |
                              grepl("\\bi.e. Salmonid stock enhancement\\b", PurposeOfMovement),]

    # label as Restocker if 'Sport/Angling', 'Restocking, i.e. Salmonid stock enhancement' is contained within the PurposeOfMovement column
    Restockerdest <- DestFarm[grepl("\\bSport/Angling\\b", PurposeOfMovement) |
                                grepl("\\bi.e. Salmonid stock enhancement\\b", PurposeOfMovement),]

    # Average tonnes per year for site- source
    Src_annualTonne <- Restockersrc[, .(AnnualTonnes = sum(TotalWeight_g, na.rm = TRUE)/1000000), by = .(Src_Code, Year)]
    SrcAverageAnnualTonnes <- Src_annualTonne[, .(averageAnnualTonnes = mean(AnnualTonnes)), by = Src_Code]

    # Average tonnes per year for site- dest
    Dest_annualTonne <- Restockerdest[, .(AnnualTonnes = sum(TotalWeight_g, na.rm = TRUE)/1000000), by = .(Dest_Code, Year)]
    DestAverageAnnualTonnes <- Dest_annualTonne[, .(averageAnnualTonnes = mean(AnnualTonnes)), by = Dest_Code]

    # combine source and dest and sum annual averages per site
    restockercomb <- merge(DestAverageAnnualTonnes, SrcAverageAnnualTonnes, by.x = "Dest_Code", by.y = "Src_Code", all = TRUE)
    restockercomb[, sum_averageAnnualTonnes := rowSums(.SD, na.rm = TRUE), .SDcols = c("averageAnnualTonnes.x", "averageAnnualTonnes.y")]

    # If 0-10 label as small Restocker
    restockercomb[, SmallRestocker := ifelse(sum_averageAnnualTonnes >=0 & sum_averageAnnualTonnes <=10, 'Y', 'N')]

    # If 10-100 label as medium Restocker
    restockercomb[, MediumRestocker := ifelse(sum_averageAnnualTonnes > 10 & sum_averageAnnualTonnes <=100, 'Y', 'N')]

    # If over 100 label as large Restocker
    restockercomb[, LargeRestocker := ifelse(sum_averageAnnualTonnes > 100, 'Y', 'N')]

    # Ongrower source & Dest --------------------------------------------------------------

    # label as Ongrower if 'Sport/Angling', 'Restocking, i.e. Salmonid stock enhancement' is contained within the PurposeOfMovement column
    Ongrowersrc <- SrcFarm[PurposeOfMovement =='Ongrowing']

    # label as Ongrower if 'Sport/Angling', 'Restocking, i.e. Salmonid stock enhancement' is contained within the PurposeOfMovement column
    Ongrowerdest <- DestFarm[PurposeOfMovement =='Ongrowing']

    # Average tonnes per year for site- Source
    Src_annualTonne <- Ongrowersrc[, .(AnnualTonnes = sum(TotalWeight_g, na.rm = TRUE)/1000000), by = .(Src_Code, Year)]
    SrcAverageAnnualTonnes <- Src_annualTonne[, .(averageAnnualTonnes = mean(AnnualTonnes)), by = Src_Code]

    # Average tonnes per year for site- Dest
    Dest_annualTonne <- Ongrowerdest[, .(AnnualTonnes = sum(TotalWeight_g, na.rm = TRUE)/1000000), by = .(Dest_Code, Year)]
    DestAverageAnnualTonnes <- Dest_annualTonne[, .(averageAnnualTonnes = mean(AnnualTonnes)), by = Dest_Code]

    # combine source and dest and sum annual averages per site
    ongrowercomb <- merge(DestAverageAnnualTonnes, SrcAverageAnnualTonnes, by.x = "Dest_Code", by.y = "Src_Code", all = TRUE)
    ongrowercomb[, sum_averageAnnualTonnes := rowSums(.SD, na.rm = TRUE), .SDcols = c("averageAnnualTonnes.x", "averageAnnualTonnes.y")]

    ## Source
    # If 0-20 label as small Ongrower
    ongrowercomb[, SmallOngrower := ifelse(sum_averageAnnualTonnes >=0 & sum_averageAnnualTonnes <=20, 'Y', 'N')]

    # If 20-50 label as medium Ongrower
    ongrowercomb[, MediumOngrower := ifelse(sum_averageAnnualTonnes > 20 & sum_averageAnnualTonnes <=50, 'Y', 'N')]

    # If over 50 label as large Ongrower
    ongrowercomb[, LargeOngrower := ifelse(sum_averageAnnualTonnes > 50, 'Y', 'N')]

    # Fishery --------------------------------------------------------------

    # Label as Fishery if Scr_AutRegStatus == 'REGFISH' (and does not contain AUTFARM)
    notSrcFarm <- lfmCategorised[!grepl("\\bAUTFARM\\b", Scr_AutRegStatus)]
    SrcFishery <- notSrcFarm[grepl("\\bREGFISH\\b", Scr_AutRegStatus)]

    #Dest
    notDestFarm <- lfmCategorised[!grepl("\\bAUTFARM\\b", Dest_AutRegStatus)]
    DestFishery <- notDestFarm[grepl("\\bREGFISH\\b", Dest_AutRegStatus)]

    # Average tonnes per year for site
    Src_annualTonne <- SrcFishery[, .(AnnualTonnes = sum(TotalWeight_g, na.rm = TRUE)/1000000), by = .(Src_Code, Year)]
    SrcAverageAnnualTonnes <- Src_annualTonne[, .(averageAnnualTonnes = mean(AnnualTonnes)), by = Src_Code]

    # Average tonnes per year for site
    Dest_annualTonne <- DestFishery[, .(AnnualTonnes = sum(TotalWeight_g, na.rm = TRUE)/1000000), by = .(Dest_Code, Year)]
    DestAverageAnnualTonnes <- Dest_annualTonne[, .(averageAnnualTonnes = mean(AnnualTonnes)), by = Dest_Code]

    # combine source and dest and sum annual averages per site
    fisherycomb <- merge(DestAverageAnnualTonnes, SrcAverageAnnualTonnes, by.x = "Dest_Code", by.y = "Src_Code", all = TRUE)
    fisherycomb[, sum_averageAnnualTonnes := rowSums(.SD, na.rm = TRUE), .SDcols = c("averageAnnualTonnes.x", "averageAnnualTonnes.y")]

    # If 0-10 label as small Fishery
    fisherycomb[, SmallFishery := ifelse(sum_averageAnnualTonnes >=0 & sum_averageAnnualTonnes <=10, 'Y', 'N')]

    # If 10-100 label as medium Fishery
    fisherycomb[, MediumFishery := ifelse(sum_averageAnnualTonnes > 10 & sum_averageAnnualTonnes <=100, 'Y', 'N')]

    # If 100-1000 label as large Fishery
    fisherycomb[, LargeFishery := ifelse(sum_averageAnnualTonnes > 100, 'Y', 'N')]


    ## Melt source and destination sites together ==================================

    # get unique sites
    sites <- data.frame(Code = c(lfmCategorised$Src_Code, lfmCategorised$Dest_Code, AnnualtonneTable$Src_Code, meanAnnualCountHatch$Src_Code),
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
      prodHatch <- meanAnnualCountHatch[Src_Code == sites$Code[i]]
      Ongrow <- ongrowercomb[Dest_Code == sites$Code[i]]
      Rest <- restockercomb[Dest_Code == sites$Code[i]]
      Fish <- fisherycomb[Dest_Code == sites$Code[i]]
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

    lfmCategorised[, Src_Category:= fcase(
      grepl("\\bAUTFARM\\b", Scr_AutRegStatus), 'Farm',
      grepl("\\bREGFISH\\b", Scr_AutRegStatus), 'Fishery',
      default = 'Other'
    )]


    lfmCategorised[, Dest_Category:= fcase(
      grepl("\\bAUTFARM\\b", Dest_AutRegStatus), 'Farm',
      grepl("\\bREGFISH\\b", Dest_AutRegStatus), 'Fishery',
      default = 'Other'
    )]

    # Production data
    production_data[, prod_Category:= fcase(
      grepl("\\bAUTFARM\\b", AuthorisationAndRegistrationStatusCode), 'Farm',
      grepl("\\bREGFISH\\b", AuthorisationAndRegistrationStatusCode), 'Fishery',
      default = 'Other'
    )]

    # melt to long format with source and destination sites
    lfmCategorised_long <- data.frame(Code = c(lfmCategorised$Src_Code,
                                               lfmCategorised$Dest_Code,
                                               production_data$Src_Code),
                                      Category = c(lfmCategorised$Src_Category,
                                                   lfmCategorised$Dest_Category,
                                                   production_data$prod_Category)) %>% unique()


    # Clean up final dataset -----------------------------------------------------------------------------------------------

    # combine with sites
    # combine with sites
    all_data <- merge(sites,
                      lfmCategorised_long,
                      by = "Code",
                      all.x = TRUE)

    # replace NA with "N"
    all_data[is.na(all_data)] <- "N"

    # remove sites which aren't in lfms
    production_list <- unique(production_data$Src_Code)
    lfmsrc <- unique(lfm_data$Src_Code)
    lfmdest <- unique(lfm_data$Dest_Code)
    lfm_list <- c(lfmdest,lfmsrc)
    lfm_final <- unique(lfm_list)

    removed_sites <- setdiff(production_list, lfm_final)

    all_data <- subset(all_data, !Code %in% removed_sites)

    # Save final dataset as csv
    utils::write.csv(all_data, here::here("outputs",
                                          scenario_name,
                                          "categorisedSites.csv"),
                     row.names = FALSE)
    return(all_data)

}


