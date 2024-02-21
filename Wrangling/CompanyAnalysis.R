# This script is for the collection of the different companies that are prevalent in the adds.
# There are different sections for different parts of the cathegorisation of the companies.
# DO NOT run the script as a whole


#### Packages ####
library(googlesheets4)
library(dplyr)
library(stringr)

#### Access information from Google sheets ####

gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")

Main <- read_sheet(sheet, "Main")

CompanyAnalysis <- read_sheet(sheet, "CompanyAnalysis")

locations_coord <- read_sheet(sheet, "locations_coord")

#### Find the unique companies ####

all_companies <- Main$Company

analysed_companies <- CompanyAnalysis$Name[-which(CompanyAnalysis$Name == 'Name of company/organisation')]

for (i in seq_len(length(all_companies))) {
  if (all_companies[i] %in% analysed_companies) {
  } else {
    analysed_companies <- append(analysed_companies,all_companies[i])
  }
}

#### Count the number of adds each company has published and record in what nations the jobs are located####

analysed_companies <- sort(analysed_companies)
nr_of_adds_for_each_company <- list()
nation_recruitment <- list()

find_nation <- function(word, df) {
  row_index <- which(df[, 2] == word)
  if (length(row_index) > 0) {
    return(df[row_index, 3])
  } else {
    return(NULL)
  }
}

for (i in seq_len(length(analysed_companies))) {
  adds <- Main %>% filter(Company == analysed_companies[i])
  nr_of_adds_for_each_company <- c(nr_of_adds_for_each_company, nrow(adds)) # Count number of adds per company
  
  # Below we first convert city location to national
  nations <- adds %>% 
    mutate(Location = case_when(tolower(str_trim(Location)) %in% tolower(locations_coord$Name2) ~ 
    locations_coord$Nation[match(tolower(str_trim(Location)), tolower(locations_coord$Name2))],
    TRUE ~ "!!!ERROR!!!")) %>% select(Location) 
  
  # Some adds are directed towards multiple cities, the nation of each city is added
  error_rows <- c()
  for (j in seq_len((nrow(nations)))) {
    if (grepl('!!!ERROR!!!', nations$Location[j], ignore.case = TRUE)) {
      #if true
      error_rows <- c(error_rows,j)
      if (grepl('UNKNOWN_LOCATION',adds$Location[j], ignore.case = TRUE)) {
        nations$Location[j] <- 'UNKNOWN_NATION'
      } else {
        cities <- strsplit(as.character(adds[j, "Location"]), ", ")[[1]]
        multi_loc <- find_nation(cities[1],locations_coord)$Nation[1]
        for (h in seq_len((length(cities)-1))) {
          multi_loc <- append(multi_loc,find_nation(cities[h+1],locations_coord)$Nation[1])
        }
        
        for (k in seq_len((length(multi_loc)))) {
          new_row <- data.frame(Location = multi_loc[k])
          nations <- rbind(nations, new_row)
        }
      }
    }
  }
  # If an ERROR occured remove that row
  if (length(error_rows>0)){
    nations <- nations[-error_rows, ] 
  }
  
  # Lastly pick the uniqe nations and add them to a string and store them in a list
  nations <- distinct(nations)
  if (nrow(nations)>1){
    nations <- nations[order(nations$Location),]
  }
  nations_chr <- nations$Location[1]
  
  if (nrow(nations)>1){
    for (l in seq_len((nrow(nations)-1))) {
    nations_chr <- paste(nations_chr,', ',nations$Location[1+l])
  }
  }
  nation_recruitment <- c(nation_recruitment, nations_chr)
}

# Make a dataframe which is to be uploaded
analysed_done <- data.frame(Name = analysed_companies, Number_of_adds = as.character(nr_of_adds_for_each_company), Location_of_jobs = as.character(nation_recruitment))

analysed_done$Organisation_type <- NA
analysed_done$Ownership_type <- NA
analysed_done$Actor_type <- NA
analysed_done$Industry_Sector <- NA
analysed_done$Value_chain_position <- NA
analysed_done$Nationality <- NA

#### Upload the newly created information to the google sheet ####
CompanyAnalysis <- rbind(CompanyAnalysis,analysed_done)
CompanyAnalysis <- CompanyAnalysis[-1,]

sheet_append(sheet, CompanyAnalysis, sheet = 'CompanyAnalysis')




#### Section barrier ####
# Nothing here

#### Create csv of companies not yet cathegorised from the google sheet ####
gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")

CompanyAnalysis <- read_sheet(sheet, "CompanyAnalysis")

not_cathegorised <- CompanyAnalysis %>% filter(is.na(Organisation_type)) %>% select(Name)

write.csv(not_cathegorised, "companies_for_AI_cathegorisation.csv")

#### After the python API-code has been run can this section be used ####

#### Adds categories of the companies to the google sheet ####

company_analysis <- read.csv("companies_for_AI_cathegorisation.csv")

merged_df <- left_join(CompanyAnalysis, company_analysis, by = "Name")

merged_df$Organisation_type <- ifelse(is.na(merged_df$Organisation_type.y), merged_df$Organisation_type.x, merged_df$Organisation_type.y)
merged_df$Ownership_type <- ifelse(is.na(merged_df$Ownership_type.y), merged_df$Ownership_type.x, merged_df$Ownership_type.y)
merged_df$Actor_type <- ifelse(is.na(merged_df$Actor_type.y), merged_df$Actor_type.x, merged_df$Actor_type.y)
merged_df$Industry_Sector <- ifelse(is.na(merged_df$Industry_Sector.y), merged_df$Industry_Sector.x, merged_df$Industry_Sector.y)
merged_df$Value_chain_position <- ifelse(is.na(merged_df$Value_chain_position.y), merged_df$Value_chain_position.x, merged_df$Value_chain_position.y)
merged_df$Nationality <- ifelse(is.na(merged_df$Nationality.y), merged_df$Nationality.x, merged_df$Nationality.y)

CompanyAnalysis <- merged_df %>%
  select(-Organisation_type.x, -Organisation_type.y, -Ownership_type.x, -Ownership_type.y,
         -Actor_type.x,-Actor_type.y,-Industry_Sector.x,-Industry_Sector.y,
         -Value_chain_position.x,-Value_chain_position.y,-Nationality.x,-Nationality.y)

write_sheet(CompanyAnalysis, ss = sheet, sheet = "CompanyAnalysis")
