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
  Scr_Restocker <- Scr_Hatchery <- Scr_Table <- Dest_Ongrower <- Dest_Table <-
    Scr_Code <- Dest_Code <- Scr_Fishery <- Dest_Fishery <- Dest_AutRegStatus <-
    Scr_Category <- Dest_Category <- CodeDescription2 <- DevCode <-
    PurposeOfMovement <- Scr_AutRegStatus <- Src_Code <- Table <- NULL

  # Classify table producers from production data ------------------------------

  # rename 'Code' col from Production to match lfm_data
  colnames(production_data)[colnames(production_data) == "Code"] <- "Src_Code"

  # label as 'Table' using production_data data if CodeDescription2 == 'Table'
  production_data[, Table := ifelse(CodeDescription2 == 'Table', 'Y', 'N')]

  # remove un-needed cols
  production_dataFarm <- production_data[, c("Src_Code", "Table"), with = FALSE]

  # Classify others using LFM data ---------------------------------------------

  ## Classify source sites =====================================================

  lfmCategorised <- lfm_data %>% data.table()

  # label as Restocker if 'Sport/Angling', 'Restocking, i.e. Salmonid stock enhancement' is contained within the PurposeOfMovement column
  lfmCategorised[, Scr_Restocker := ifelse(PurposeOfMovement %in% c('Sport/Angling',
                                                                    'Restocking, i.e. Salmonid stock enhancement'), 'Y', 'N')]

  # label as Hatchery if DevCode = 'EGG'
  lfmCategorised[, Scr_Hatchery := ifelse(DevCode == 'EGG', 'Y', 'N') ]

  # label as Table if PurposeOfMovement is equal to 'Slaughter'
  lfmCategorised[, Scr_Table := ifelse(PurposeOfMovement == 'Slaughter', 'Y', 'N')]

  ## Classify destination sites ================================================

  # label as Ongrower if 'Ongrowing', 'Broodstock' is contained within the PurposeOfMovement column
  lfmCategorised[, Dest_Ongrower := ifelse(PurposeOfMovement %in% c('Ongrowing',
                                                                    'Broodstock'), 'Y', 'N')]

  # label as Table if PurposeOfMovement is equal to 'Slaughter'
  lfmCategorised[, Dest_Table := ifelse(PurposeOfMovement == 'Slaughter', 'Y', 'N')]

  ## Melt source and destination sites together ==================================

  # get unique sites
  sites <- data.frame(Code = c(lfm_data$Src_Code, lfm_data$Dest_Code),
                      Hatchery = NA,
                      Ongrower = NA,
                      Restocker = NA,
                      Table = NA,
                      Fishery = NA) %>% unique()

  # loop to get types
  for(i in 1:nrow(sites)){
    prod <- production_dataFarm[Src_Code == sites$Code[i]]
    source <- lfmCategorised[Src_Code == sites$Code[i]]
    dest <- lfmCategorised[Dest_Code == sites$Code[i]]
    # Hatchery
    if("Y" %in% source$Scr_Hatchery){
      sites$Hatchery[i] <- "Y"
    }
    # Ongrower
    if("Y" %in% dest$Dest_Ongrower){
      sites$Ongrower[i] <- "Y"
    }
    # Restocker
    if("Y" %in% source$Scr_Restocker){
      sites$Restocker[i] <- "Y"
    }
    # Table
    if("Y" %in% prod$Table ||
       "Y" %in% source$Scr_Table ||
       "Y" %in% dest$Dest_Table){
      sites$Table[i] <- "Y"
    }
  }

  # Classify using RegStatus ---------------------------------------------------

  # Label as farm if AutRegStatus is equal to 'REGFISH'
  lfmCategorised[, Scr_Fishery := ifelse(Scr_AutRegStatus == 'REGFISH', 'Y', 'N') ]
  lfmCategorised[, Dest_Fishery := ifelse(Dest_AutRegStatus == 'REGFISH', 'Y', 'N') ]

  # loop to add to type
  for(i in 1:nrow(sites)){
    source <- lfmCategorised[Src_Code == sites$Code[i]]
    dest <- lfmCategorised[Dest_Code == sites$Code[i]]
    # Fishery
    if("Y" %in% source$Scr_Fishery ||
       "Y" %in% dest$Dest_Fishery){
      sites$Fishery[i] <- "Y"
    }
  }

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

  # combine with sites
  all_data <- merge(sites,
                    lfmCategorised_long,
                    by = "Code",
                    all.x = TRUE)

  # replace NA with "N"
  all_data[, c("Hatchery",
               "Ongrower",
               "Restocker",
               "Table",
               "Fishery")][is.na(all_data[c("Hatchery",
                                            "Ongrower",
                                            "Restocker",
                                            "Table",
                                            "Fishery")])] <- "N"

  # print head
  print(head(all_data))

  # Save final dataset as csv
  utils::write.csv(all_data, here::here("outputs",
                                         scenario_name,
                                         "categorisedSites.csv"),
                   row.names = FALSE)
  return(all_data)

}
