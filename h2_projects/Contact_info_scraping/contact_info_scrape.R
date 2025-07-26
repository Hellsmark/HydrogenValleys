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



#Define what data points we want to extract
output_structure <- type_array(
  items = type_object(
    name = type_string("Name of person"),
    title = type_string("Job title of that person"),
    email = type_string("That person's email address"),
    phone = type_string("That person's phone number")))

#List to store results
results<- list()


for (i in 3530:3533){ 
  chat <- chat_openai(
    system_prompt = "Extract contact information to the people mentioned in the 
  texts i show you. Create a row for each person mentioned in the text.")
  Sys.sleep(3)
  
  
  if (!is.na(small_df$description[i]) && nchar(small_df$description[i]) <= 12000){ #Don't read adds if they are too long
    extracted_data <- chat$extract_data(
      small_df$description[i],
      type = output_structure)
    
    if (nrow(extracted_data) > 0) {
      extracted_data$company <- small_df$company[i]
      results[[i]] <- extracted_data
      print(i)
      
    } else {
      results[[i]] <- extracted_data
      print(i)
    }
  } else {
    print("too big")
  }
}



clean_contacts <- function(results, df) {
  final_df <- bind_rows(results) %>%  #Combine all the results into a final df.
    filter(!name %in% df$company)%>%  # Remove row if the name is a company name
    #filter(!str_detect(tolower(title), "recruiting chef|recruiter|recruitment manager|recruiting manager|rekryterande chef|rekryterare|recruitment contact")) %>%  # Filter some titles
    mutate(name = str_remove(name, "^(Prof\\.|Docent|Assistant Professor)\\s+"))%>% #removes titles in front of name
    distinct(name, .keep_all = TRUE) %>%  # Remove duplicates
    return(final_df)
}

batch23<- clean_contacts(results, df)


write.csv(batch23, "C:/Users/Jakob Westerback/OneDrive/Skrivbord/ESA - jobb/HydrogenValleys/h2_projects/Contact_info_scraping/contacts_from_description.csv")

final <- rbind(batch, batch1)
