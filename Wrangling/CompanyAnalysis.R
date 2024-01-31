# This script is for the collection of the different companies that are prevalent in the adds.

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


