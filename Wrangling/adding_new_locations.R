# This script is for adding new locations

# This script should not be run as a whole. Up until # Load locations # can be run 
# But then must the locations be added manually to the sheet 
# Then can the rest be run to update the sheet with the new locations

#### Packages ####
library(googlesheets4)
library(dplyr)

#### Access to google sheet ####

# Authorize googlesheets4  to access your Google Sheets and Drive
# Please note that you need to have a Google account and be signed in to it in your web browser for it to work

gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

# Get the Google Sheet by its name within the shared folder
sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")

#### Data loading ####

main <- read_sheet(sheet, "Main") 
adds <- main %>% select(ID,Location)

se_loc <- read_sheet(sheet, "se_scrape") %>% select(IDcol,location)
colnames(se_loc) <- c("ID","Location")

no_loc <- read_sheet(sheet, "no_scrape") %>% select(IDcol,sted)
colnames(no_loc) <- c("ID","Location")

dk_loc <- read_sheet(sheet, "dk_scrape") %>% select(IDcol,location)
colnames(dk_loc) <- c("ID","Location")

#### Finding missing locations ####
missingLocations <- adds %>% filter(Location == "!!!NEW_LOCATION!!!")

names_of_missing <- rbind(merge(se_loc, missingLocations["ID"], by = "ID"),merge(no_loc, missingLocations["ID"], by = "ID"),merge(dk_loc, missingLocations["ID"], by = "ID"))

#### Load locations ####
locations <- read_sheet(sheet, "locations_coord")

#### Add missing locations to sheet ####
for (i in seq_len(nrow(names_of_missing))) {
  nr_matching <- 0
  for (j in seq_len(nrow(locations))) {
    if (grepl(locations$Name[j], names_of_missing$Location[i], ignore.case = TRUE)) {
      #if true
      match <- locations$Name2[j] # save matching location name
      nr_matching <- nr_matching+1 # document how many matches we get
    } 
  }
  if (nr_matching == 0) {
    # if no matches were found add this point it means that no location has been given
    missingLocations$Location[i] <- "!!!UNKNOWN_LOCATION!!!"
  } else if (nr_matching > 1) {
    # if more than 1 match was found will the location choice have to be made manually
    missingLocations$Location[i] <- "!!!MULTIPLE_LOCATIONS!!!"
  } else {
    # when nr of matches is not 0 nor more than 1 will the found match be assigned to the add
    missingLocations$Location[i] <- match 
  }
}

#### Update sheet with new locations ####

adds$Location[match(missingLocations$ID, adds$ID)] <- missingLocations$Location

main <- main %>%
  mutate(ID = ifelse(ID %in% adds$ID, adds$ID[match(ID, adds$ID)], ID),
         Location = ifelse(ID %in% adds$ID, adds$Location[match(ID, adds$ID)], Location))

# Write the updated dataframe back to the 'Main' worksheet
write_sheet(main, ss = sheet, sheet = "Main")
