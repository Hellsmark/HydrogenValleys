#This script is used when we have to manually update new company names.
#OBS! YOU NEED TO RUN THIS SCIPT IN TWO PARTS WHERE YOU MANUALLY UPDATE THE COMPANY_NAMES WORK SHEET IN BETWEEN.

#### Packages ####

library(data.table)
library(dplyr)
library(tidyr)
library(dplyr)
library(googledrive)
library(googlesheets4)
library(stringr)
library(base64enc)

#### Preparations to use Google-saved files ####

# Authorize googlesheets4 and googledrive to access your Google Sheets and Drive
# Please note that you need to have a Google account and be signed in to it in your web browser for it to work
#drive_auth()
gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

# Get the Google Sheet by its name within the shared folder
sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")

#### Finding the ads with "new" companies ####
main <- read_sheet(sheet, "Main")
new_comp <- main %>% filter(Company == "!!!NEW_COMPANY!!!") %>% select(ID,Company)

#From new_comp you get the IDs for the ads with "new" companies and can manually add these
#to the company_names work sheet in the google file

#get the work sheets with the scrape data and combine them into one df with the selected comlumns ID and Company
se_comp <- read_sheet(sheet,"se_scrape") %>% select(IDcol,company) %>% rename(ID = IDcol, Company = company)
no_comp <- read_sheet(sheet,"no_scrape") %>% select(IDcol,arbetsgivare) %>% rename(ID = IDcol, Company = arbetsgivare)
dk_comp <- read_sheet(sheet,"dk_scrape") %>% select(IDcol,company) %>% rename(ID = IDcol, Company = company)
all_comp <- bind_rows(se_comp, no_comp, dk_comp)

new_comp_names <- all_comp %>% filter(ID %in% new_comp$ID) %>% select(ID, Company)

# How to de-compress
#dk_decomp <- read_sheet(sheet,"dk_scrape") %>% filter(IDcol %in% new_comp$ID) %>% select(IDcol,company,all_text1) 
#decompress_string <- function(x) {
#  decompressed <- memDecompress(base64decode(x), type = "gzip")
#  rawToChar(decompressed)
#}

#dk_decomp$all_text1 <- lapply(dk_decomp$all_text1, decompress_string)

 #After manually adding the new companies, we get the new names from company_names  
####DO NOT RUN THE CODE BELOW BBEFORE MANUALLY UPDATING THE GOOGLE SHEET ####

#### Renaming the updated company names ####

#reading the main work sheet again, if some of the company names has been updated manually
main <- read_sheet(sheet, "Main")
new_comp <- main %>% filter(Company == "!!!NEW_COMPANY!!!") %>% select(ID,Company)

#get the (updated) worksheet with old and new names
comp_names <- read_sheet(sheet, "company_names")

#Update the company names with the company_names work sheet. Companies that still cant be found will be named !!!NEW_COMPANY!!! and there can still be some companies named !!!UNKNOWN_COMPANY!!! 
to_add <- all_comp %>% filter(ID %in% new_comp$ID) %>% mutate(Company = case_when(tolower(str_trim(Company)) %in% tolower(comp_names$Old_name) ~ 
                                                                                            comp_names$New_name[match(tolower(str_trim(Company)), tolower(comp_names$Old_name))],
                                                                                          TRUE ~ "!!!NEW_COMPANY!!!"))

#### Data upload ####

# Update the 'ID' and 'Company' columns in the 'Main' worksheet
main <- main %>%
  mutate(ID = ifelse(ID %in% to_add$ID, to_add$ID[match(ID, to_add$ID)], ID),
         Company = ifelse(ID %in% to_add$ID, to_add$Company[match(ID, to_add$ID)], Company))

# Write the updated dataframe back to the 'Main' worksheet
write_sheet(main, ss = sheet, sheet = "Main")