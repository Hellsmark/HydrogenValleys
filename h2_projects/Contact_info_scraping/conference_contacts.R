library(ellmer)
library(tidyverse)
library(tidyverse)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(stringr)
    
ss <- "https://docs.google.com/spreadsheets/d/1bzxpQRAl9xTRNk22ycRZcBWIcX692jQmwAtRVUsR5xw/edit?gid=1093263258#gid=1093263258"

df_raw <- read_sheet(ss)
df <- df_raw %>% janitor::clean_names()
df <- df %>% select(name, job_title, organisation)

df[is.na(df)] <- ""
df$new <- apply(df, 1, paste, collapse = ",")

OPENAI_API_KEY<- Sys.getenv("OPENAI_API_KEY")



output_structure <- type_array(
  items = type_object(
    email = type_string("The person's email address"),
    phone = type_string("The person's phone number")))


######################################## GATHER DATA ###############################################

#This part gathers all the information, might take around 5 minutes to run
results <- list()
for (i in 1:213){
  chat <- chat_openai(system_prompt = "For each person, find their work contact information")
  Sys.sleep(3)
  
  extracted_data <- chat$extract_data(
    df$new[i],
    type=output_structure)
  results[[i]]<-extracted_data
  print(i)
}

final<- bind_rows(results)
base<- (df[1:213,1:3])   #Creates a df with names and roles to merge with the information

#Merging the names/title/company with contact details
total<-cbind(base,finall) #Fill this one with results from first round

write_sheet(total, ss, sheet="Final_conf")

############################### USE THIS CODE IF THE LENGTH OF "FINAL" IS NOT AS LONG AS "BASE"####################

#Because of some rows in the result being 2x2 data instead of 1x2, i use this code to make a row for each person
finall <- map_dfr(results, function(x) {
  as_tibble(matrix(unlist(x), nrow = 1, byrow = TRUE))
})

