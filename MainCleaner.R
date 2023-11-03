# This script will be our main "cleaning script" which will take the information from the scrapes and upload them into a cleansed Google-sheet file.
# It could really just as well be saved as a csv and shared via another method, but for now we will use Google Drive as the sharing method for the adds. 

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



#### Preparations to use Google-saved files ####

# Authorize googlesheets4 and googledrive to access your Google Sheets and Drive
# Please note that you need to have a Google account and be signed in to it in your web browser for it to work
drive_auth()
gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

# Get the shared folder by its name
folder <- drive_get("WP3-H2")

# Get the Google Sheet by its name within the shared folder
sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")


#### end ####

# Here one will load the latest scrape as a dataframe which then can be used for updating the Main datafile
# Also the previous uploading of data will be loaded, in order for a comparison of what is new
# We will count the number of adds already upploaded to the Main google sheet and begin the new ideas from there

new_data <- data.frame( ID = numeric(0), Title = numeric(0), Company = numeric(0), 
                        Location = numeric(0), Description = numeric(0), Scrape_date = numeric(0))

#### Swedish data ####
ams_Latest_scrape <- read.csv("ams.csv", sep=";") %>% filter(s_terms.i.=="vätgas*")

# Get the specific worksheet
se_Old <- read_sheet(sheet, "se_scrape")

#Compare what is new
new_se <- ams_Latest_scrape[!ams_Latest_scrape$links %in% se_Old$links,] 

# Add ID to our new data
ID <- nrow(se_Old)
IDcol <- c()
for (i in seq_len(nrow(new_se))) {
  ID <- ID + 1
  IDcol <- append(IDcol,10000+ID)
}

new_se <- cbind(IDcol,new_se)

new_for_clean_se <- new_se %>% select(IDcol,headline,company,location,description,scrape_date)

colnames(new_for_clean_se) <- c("ID","Title", "Company","Location","Description","Scrape_date")

# Add to total of new data
new_data <- rbind(new_data,new_for_clean_se)



#### Norwegian data ####
finn_Latest_scrape <- read.csv("finn_no_h2.csv")

# Get the specific worksheet
no_Old <- read_sheet(sheet, "no_scrape")

#Compare what is new
new_no <- finn_Latest_scrape[!finn_Latest_scrape$url %in% no_Old$url,] 

# Add ID to our new data
ID <- nrow(no_Old)
IDcol <- c()
for (i in seq_len(nrow(new_no))) {
  ID <- ID + 1
  IDcol <- append(IDcol,20000+ID)
}

new_no <- cbind(IDcol,new_no)

new_for_clean_no <- new_no %>% select(IDcol,stillingstittel,arbetsgivare,sted,add_text,id)

# We want the date for the scrape to be in year-month-day

for (i in seq_len(nrow(new_for_clean_no))) {
  new_for_clean_no$id[i] <- substr(new_for_clean_no$id[i], start = 1, stop = 10)
}

colnames(new_for_clean_no) <- c("ID","Title", "Company","Location","Description","Scrape_date")

# Add to total of new data
new_data <- rbind(new_data,new_for_clean_no)



#### Danish data ####
dk_Latest_scrape <- as.data.frame(readRDS("dk_h2.rds")) #%>% select(!all_text)

# We have an issue with the danish file. It is the "all_text" column which contains cells containing more than 50k characters (which is the limit of google sheet)
# Define a function to compress a string
compress_string <- function(x) {
  compressed <- memCompress(charToRaw(x), type = "gzip")
  base64encode(compressed)
}

# Apply the function to the column in dataframe
dk_Latest_scrape$all_text <- lapply(dk_Latest_scrape$all_text, compress_string)

# Get the specific worksheet
dk_Old <- read_sheet(sheet, "dk_scrape")

#Compare what is new
new_dk <- dk_Latest_scrape[!dk_Latest_scrape$link_to_external_add %in% dk_Old$link_to_external_add,] 

# Add ID to our new data
ID <- nrow(dk_Old)
IDcol <- c()
for (i in seq_len(nrow(new_dk))) {
  ID <- ID + 1
  IDcol <- append(IDcol,30000+ID)
}

new_dk <- cbind(IDcol,new_dk)

new_for_clean_dk <- new_dk %>% select(IDcol,title,company,location,text,id)

# We want the date for the scrape to be in year-month-day

for (i in seq_len(nrow(new_for_clean_dk))) {
  new_for_clean_dk$id[i] <- substr(new_for_clean_dk$id[i], start = 1, stop = 10)
}

colnames(new_for_clean_dk) <- c("ID","Title", "Company","Location","Description","Scrape_date")

# Add to total of new data
new_data <- rbind(new_data,new_for_clean_dk)



#### Clean & rename companies ####

comp_names <- read_sheet(sheet, "company_names")

data_new_names <- new_data %>% mutate(Company = case_when(tolower(Company) %in% tolower(comp_names$Old_name) ~ 
                                                            comp_names$New_name[match(Company, comp_names$Old_name)], TRUE ~ Company))




#hello world


# When the new_data df has been cleaned with good names for location and company the df should be uploaded to the google sheet in the work sheet "main"
# then should also the new_se/no/dk files also be uploaded and appended to their sepserate scrape sheets in the google sheet


#########

# How to de-compress

#decompress_string <- function(x) {
#  decompressed <- memDecompress(base64decode(x), type = "gzip")
#  rawToChar(decompressed)
#}


#dk_Latest_scrape$all_text <- lapply(dk_Latest_scrape$all_text, decompress_string)


# An old thing
# number_of_uploaded <- sum(apply(IDcol, 1, function(x) all(x != "")))

