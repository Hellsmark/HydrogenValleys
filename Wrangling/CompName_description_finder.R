# This script will be used to fill in company names for those with lacking names in the company column. The description will be searched through and when the company name has been found it will add it to the column

#### Packages ####

library(googlesheets4)
library(dplyr)
library(base64enc)

#### Google sheet access ####

# Authorize googlesheets4  to access your Google Sheets and Drive
# Please note that you need to have a Google account and be signed in to it in your web browser for it to work

gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

# Get the Google Sheet by its name within the shared folder
sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")

#### Data loading ####

adds <- read_sheet(sheet, "Main") %>% select(ID,Company)
compNames <- read_sheet(sheet, "company_names") %>% select(Old_name,New_name) %>% filter(New_name != "!!!UNKNOWN_COMPANY!!!")

se_desc <- read_sheet(sheet,"se_scrape") %>% select(IDcol,description)
no_desc <- read_sheet(sheet,"no_scrape") %>% select(IDcol,add_text)
dk_desc <- read_sheet(sheet,"dk_scrape") %>% select(IDcol,all_text1,all_text2,all_text3)

dk_desc$all_text <- paste0(dk_desc$all_text1, dk_desc$all_text2, dk_desc$all_text3) 
dk_desc <- dk_desc[,c("IDcol","all_text")]

# How to de-compress
decompress_string <- function(x) {
  decompressed <- memDecompress(base64decode(x), type = "gzip")
  rawToChar(decompressed)
}

dk_desc$all_text <- lapply(dk_desc$all_text, decompress_string)

#### CompName searcher ####

missingNames <- adds %>% filter(Company == "!!!UNKNOWN_COMPANY!!!")

# Function to check if string is in the other strings
for (i in seq_len(nrow(missingNames))) {
  nr_matching <- 0
  if (missingNames$ID[i] > 30000) {
    desc <- dk_desc %>% filter(IDcol == missingNames$ID[i])
    desc <- desc$all_text[1]
  } else if (missingNames$ID[i] > 20000) {
    desc <- no_desc %>% filter(IDcol == missingNames$ID[i])
    desc <- desc$add_text[1]
  } else {
    desc <- se_desc %>% filter(IDcol == missingNames$ID[i])
    desc <- desc$description[1]
  }
  #-----#
  for (j in seq_len(nrow(compNames))) {
    if (grepl(paste0("\\b",compNames$Old_name[j],"\\b"), desc)) {
      #if true
      match <- compNames$New_name[j] # save matching company name name
      nr_matching <- nr_matching+1 # document how many matches we get
      break
    } 
  }
  if (nr_matching == 0) {
    # if no matches were found must the company list be updated to include more company names
    missingNames$Company[i] <- "!!!NEW_COMPANY!!!"
  } else {
    # when nr of matches is not 0 will the found match be assigned to the add
    missingNames$Company[i] <- match 
  }
}
#### DOUBLE CHECK #### 
#The following companies have (more than once) been given to the wrong ads:

#e.on (should have been Econ instead)
#Falbygdens energi (should have been Nordion Energi)
#KTH
#DNV
#Aalborg Universitet

#### Data upload ####

# We update the adds and upload to the google sheet in the work sheet "main"

adds$Company[match(missingNames$ID, adds$ID)] <- missingNames$Company

main <- read_sheet(sheet, "Main")

# Update the 'ID' and 'Company' columns in the 'Main' worksheet
main <- main %>%
  mutate(ID = ifelse(ID %in% adds$ID, adds$ID[match(ID, adds$ID)], ID),
         Company = ifelse(ID %in% adds$ID, adds$Company[match(ID, adds$ID)], Company))

# Write the updated dataframe back to the 'Main' worksheet
write_sheet(main, ss = sheet, sheet = "Main")
