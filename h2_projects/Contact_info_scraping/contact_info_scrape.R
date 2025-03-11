library(ellmer)
library(tidyverse)
library(tidyverse)
library(googlesheets4)
library(dplyr)
library(ggplot2)

OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

ss <- "https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit?gid=0#gid=0"

# Read project sheet and clean
df_raw <- read_sheet(ss, sheet = "Main")
df <- df_raw %>% janitor::clean_names()

chat<- chat_openai(
  system_prompt = "Extract contact information to the people mentioned in the 
  texts i show you. Create a row for each person mentioned in the text. DO NOT include
  the contacts with the title: reqruiter, rekryterande chef, rekryterare, recruitment contact 
  or anything similair.")

output_structure <- type_array(
  items = type_object(
    company = type_string("Name of company"),
    name = type_string("Name of person"),
    title = type_string("Job title of that person"),
    email = type_string("That person's email address"),
    phone = type_string("That person's phone number")))

# Här körs allting! Lämnar in alla texter till Chat_gpt för att extrahera info
# Tar en minut för 20 annonser... :/  ändra (df$description[1]) när du ska köra alla
results <- lapply(df$description[1], function(text) {
  chat$extract_data(
    text,
    type = output_structure)})

# Lägger in alla uppgifter i en dataframe, tar bort personer som återkommer samt 
# alla kontakter som har rekryterande roller
results_df <- bind_rows(results) %>%
  distinct(name, .keep_all = TRUE) %>%
  filter(!str_detect(tolower(title),
  "recruiter|rekryterande chef|rekryterare|recruitment contact"))


