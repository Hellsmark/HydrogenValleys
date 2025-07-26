library(ellmer)
library(tidyverse)
library(googlesheets4)
library(janitor)
library(dplyr)
library(stringr)

# Next step here is to count how many contacts from each category 
# we have for the three different categorical systems.

ss <-"https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit?gid=1724227169#gid=1724227169"

df <- read_sheet(ss,sheet= "company_category")
categories <-df %>%janitor::clean_names()

df <- read_sheet(ss,sheet= "contacts_projects")
df_cont<-df %>%janitor::clean_names()

df_cat <- categories %>% mutate(actor = str_to_lower(actor)) %>% distinct(actor, .keep_all = TRUE)
df_cont <- df_cont %>% mutate(actor = str_to_lower(org))

df1 <- df_cont %>%
  left_join(df_cat, by = "actor")

df1<-df1 %>% select(-actor)


#Count categories

df1%>%
  count(four_categories, sort=TRUE)

df1%>%
  count(five_categories, sort=TRUE)

df1%>%
  count(eight_categories, sort=TRUE)

write_sheet(df1,ss, sheet="contacts_categories")


# The results here makes me think that we should try to see
# if we can re-assign some "producers" so 
