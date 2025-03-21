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
    email = type_string("The person's email address"),
    phone = type_string("The person's phone number")))


########### This part will run 2 times, the end will to fill out data gaps ###############################################

#This part gathers all the information, might take around 5 minutes to run
results <- list()
for (i in 1:213){
  extracted_data <- chat$extract_data(
    df$new[i],
    type=output_structure)
  results[[i]]<-extracted_data
}


final<- bind_rows(results)
base<- (df[1:213,1:3])   #Creates a df with names and roles to merge with the information

#Merging the names/title/company with contact details
total<-cbind(base,final) #Fill this one with results from first round
total2<-cbind(base,final) # Fill this with results from second round


############################### Fill out the gaps #########################################33
# When a spot is empty in one, use the data from the other.

for (i in 1:nrow(total2)){
  if (total2$email[i]==""){
    total2$email[i]<-total$email[i]
  }
  if(total2$phone[i]=="" || total2$phone[i]=="N/A" || total2$phone[i]=="Not available"){
    total2$phone[i]<-total$phone[i]
  }
}

print(total2$phone)

