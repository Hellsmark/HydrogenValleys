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

chat <- chat_openai(system_prompt = "For each person, find their work contact information")


output_structure <- type_array(
  items = type_object(
    email = type_string("That person's email address"),
    phone = type_string("That person's phone number")))

results <- list()

for (i in 1:10){
  extracted_data <- chat$extract_data(
    df$new[i],
    type=output_structure)
  results[[i]]<-extracted_data
}


clean_contacts <- function(results) {
  final_df <- bind_rows(results) %>%  #Combine all the results into a final df.
    distinct() %>%  # Remove duplicates
    return(final_df)
}

cl<- clean_contacts(results)

#cl2<- clean_contacts(results)

combin<- cl %>% left_join(cl2)


extracted_data<-df %>% slice(1:10) %>%select(name, job_title, organisation)
print(df[1,])
