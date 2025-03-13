library(ellmer)
library(tidyverse)
library(tidyverse)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(stringr)

OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

ss <- "https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit?gid=0#gid=0"

# Read project sheet and clean
df_raw <- read_sheet(ss, sheet = "Main")
df <- df_raw %>% janitor::clean_names()
small_df <- df%>% select(company,description)


#prompt
chat<- chat_openai(
  system_prompt = "Extract contact information to the people mentioned in the 
  texts i show you. Create a row for each person mentioned in the text.")

#Define what data points we want to extract
output_structure <- type_array(
  items = type_object(
    name = type_string("Name of person"),
    title = type_string("Job title of that person"),
    email = type_string("That person's email address"),
    phone = type_string("That person's phone number")))

#List to store results
results<- list()

#insert every text in chat_gpt and store results as lists in "results"
for (i in 20:40){
  extracted_data <- chat$extract_data(
    small_df$description[i],
    type=output_structure)
  extracted_data$company<- small_df$company[i]
  results[[i]]<- extracted_data
}

clean_contacts <- function(results, df) {
  final_df <- bind_rows(results) %>%  #Combine all the results into a final df.
    filter(!name %in% df$company)%>%  # Remove name if it's a company name
    filter(!str_detect(tolower(title), "recruiting chef|recruiter|recruitment manager|recruiting manager|rekryterande chef|rekryterare|recruitment contact")) %>%  # Filter some titles
    mutate(name = str_remove(name, "^(Prof\\.|Docent|Assistant Professor)\\s+"))%>% #removes titles in front of name
    distinct(name, .keep_all = TRUE) %>%  # Remove duplicates
    
    return(final_df)
}

cleans<- clean_contacts(results, df)

