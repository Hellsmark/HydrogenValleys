#### Packages ####

library(data.table)
library(dplyr)
library(tidyr)
library(dplyr)
library(googledrive)
library(googlesheets4)
library(zlib)
library(digest)
library(openssl)
library(base64enc)
library(stringr)

#### Preparations to use Google-saved files ####

# Authorize googlesheets4 and googledrive to access your Google Sheets and Drive
# Please note that you need to have a Google account and be signed in to it in your web browser for it to work
drive_auth()
gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

# Get the Google Sheet by its name within the shared folder
sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")

#### Finding the ads with "new" companies ####
main <- read_sheet(sheet, "Main")
new_comp <- filter(Company == "!!!NEW_COMPANY!!!") %>% select(ID,Company)
#From new_comp you get the IDs for the ads with "new" companies and can manually add these
#to the company_names sheet in the google file

#After manually adding the new companies the Main sheet needs to be updated with the new names 

#### Data upload ####

# We update the adds and upload to the google sheet in the work sheet "main"

#get the worksheet with old and new names
comp_names <- read_sheet(sheet, "company_names")

#Replaces the names of the companies in new_data with the new names from the worksheet, if there is no match it will be named !!!NEW_COMPANY!!!
#we will fix both !!!NEW_COMPANY!!! and the rows with !!!UNKNOWN_COMPANY!!! in a seperate script
data_new_names <- new_data %>% mutate(Company = case_when(tolower(str_trim(Company)) %in% tolower(comp_names$Old_name) ~ 
                                                            comp_names$New_name[match(tolower(str_trim(Company)), tolower(comp_names$Old_name))],
                                                          TRUE ~ "!!!NEW_COMPANY!!!"))


# Update the 'ID' and 'Company' columns in the 'Main' worksheet
main <- main %>%
  mutate(ID = ifelse(ID %in% adds$ID, adds$ID[match(ID, adds$ID)], ID),
         Company = ifelse(ID %in% adds$ID, adds$Company[match(ID, adds$ID)], Company))

# Write the updated dataframe back to the 'Main' worksheet
write_sheet(main, ss = sheet, sheet = "Main")