# This script will be our main "cleaning script" which will take the information from the scrapes and upload them into a cleansed Google-sheet file.
# It could really just as well be saved as a csv and shared via another method, but for now we will use Google Drive as the sharing method for the adds. 

#### Packages ####

library(data.table)
library(dplyr)
library(tidyr)
library(dplyr)
library(googledrive)
library(googlesheets4)

#### end ####

#### Preparations to use Google-saved files ####

# Authorize googlesheets4 and googledrive to access your Google Sheets and Drive
# Please note that you need to have a Google account and be signed in to it in your web browser for
drive_auth()
gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

# Get the shared folder by its name
folder <- drive_get("WP3-H2")

# Get the Google Sheet by its name within the shared folder
sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")


#### end ####

# Here one will load the latest scrape as a dataframe which then can be used for updating the Main datafile
# Also the previous uploading of data will be loaded in order for a comparison of what is new

new_data <- data.frame( Title = numeric(0), Company = numeric(0), Location = numeric(0), Description = numeric(0), Scrape_date = numeric(0))

#### Swedish data ####
ams_Latest_scrape <- read.csv("ams.csv", sep=";") %>% filter(s_terms.i.=="vätgas*")

# Get the specific worksheet
se_Old <- read_sheet(sheet, "se_scrape")

#Compare what is new
new_se <- ams_Latest_scrape[!ams_Latest_scrape$links %in% se_Old$links,] %>% select(headline,company,location,description,scrape_date)

colnames(new_se) <- c("Title", "Company","Location","Description","Scrape_date")

# Add to total of new data
new_data <- rbind(new_data,new_se)

#### end ####

#### Norwegian data ####
finn_Latest_scrape <- read.csv("finn_no_h2.csv")

# Get the specific worksheet
no_Old <- read_sheet(sheet, "no_scrape")

#Compare what is new
new_no <- finn_Latest_scrape[!finn_Latest_scrape$url %in% no_Old$url,] %>% select(titel,arbetsgivare,sted,add_text,id)

colnames(new_no) <- c("Title", "Company","Location","Description","Scrape_date")

# Add to total of new data
new_data <- rbind(new_data,new_no)

#### end ####

#### Danish data ####
dk_Latest_scrape <- as.data.frame(readRDS("/Users/viktorrosenberg/Documents/Jobb/Chalmers/dk_h2.rds")) #%>% select(!all_text)

# Get the specific worksheet
dk_Old <- read_sheet(sheet, "dk_scrape")

#Compare what is new
new_dk <- dk_Latest_scrape[!dk_Latest_scrape$link_to_external_add %in% dk_Old$link_to_external_add,] %>% select(title,company,location,text,id)

colnames(new_no) <- c("Title", "Company","Location","Description","Scrape_date")

# Add to total of new data
new_data <- rbind(new_data,new_dk)

#### end ####













