library(tidyverse)
library(googlesheets4)
library(dplyr)
library(ggplot2)



ss <- "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit?gid=1138676736#gid=1138676736"
ss_jw <- "https://docs.google.com/spreadsheets/d/14Yv8uc_-WGARJ0vLzMbMXplSMIjiRxnAXwetjH6xCU8/edit?gid=1026439614#gid=1026439614"

# Read project sheet and clean
df_raw <- read_sheet(ss, sheet = "Projects_2")
df <- df_raw %>% janitor::clean_names()


# extract all collaborations and unique company names. Note that these names need to be harmonized with existing list
collaborations_raw <- df %>% 
  select(id, project_name, confirmed_partners) %>% 
  separate_rows(confirmed_partners, sep = ", ") %>%
  mutate(confirmed_partners = str_squish(confirmed_partners)) 

check_collab <- read_sheet(ss_jw,sheet = "Blad4")


unique <- collaborations_raw %>%
  distinct(confirmed_partners) %>%
  rename(Old_name = confirmed_partners) # Rename 'confirmed_partners' to 'Old_name'


unique <- unique %>%
  left_join(check_collab, by = "Old_name") %>%
  select(Old_name,New_name, Comment)

write_sheet(unique, ss, sheet = "new_names")

######################################## COLLABORATIONS ####################################################

library(tidyverse)
library(googlesheets4)

# project sheet
ss <- "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit?gid=1138676736#gid=1138676736"

# Read project sheet and clean
df_raw <- read_sheet(ss, sheet = "Projects_2")
df <- df_raw %>% janitor::clean_names()


id_comp <- df %>%
  select(id, project_name, confirmed_partners) %>%
  separate_rows(confirmed_partners, sep= ", ")%>%
  mutate(confirmed_partners = str_squish(confirmed_partners)) 

new_names <- read_sheet(ss,sheet="new_names")
id_comp_new <- id_comp %>%
  left_join(new_names, by=c("confirmed_partners"="Old_name"))

id_comp_new <- subset(id_comp_new, select = -confirmed_partners) %>%
  rename("Comp_name"="New_name")
id_comp_new <- id_comp_new %>% distinct(id, Comp_name, .keep_all = TRUE)
write_sheet(id_comp_new,ss, sheet="collaborations")

#################################################### UNIQUE ACTORS ########################################

unique <- new_names%>%
  select(New_name)%>%
  distinct()

write_sheet(unique,ss,sheet="actors")
#################################################### REFERENCES ########################################

ref<- df %>%
  select(id,project_name,contains("ref"))%>%
  pivot_longer(contains("ref"), names_to = "ref", values_to = "url") %>%
  na.omit()
write_sheet(ref,ss,sheet="references")


