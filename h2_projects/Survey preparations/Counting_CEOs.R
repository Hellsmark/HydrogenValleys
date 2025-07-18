## This code is just for counting the ammounts of CEOs in our contacts to see
# what type of people will answer out survey.

library(tidyverse)
library(dslabs)
library(googlesheets4)
library(ggplot2)

ss <- "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit?gid=1946686801#gid=1946686801"

df_raw<- read_sheet(ss, sheet="contacts_projects")
df <- df_raw%>% janitor::clean_names()

roles <- df %>% select(role, contains("CEO"))


ceo_count <- df %>% summarise(n_CEO = sum(str_detect(role, "CEO|CTO|CFO|VD|Pressident|COO"), na.rm = TRUE))
ceo_count

