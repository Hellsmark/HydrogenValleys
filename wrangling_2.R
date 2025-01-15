library(tidyverse)
library(googlesheets4)

# project sheet
ss <- "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit?gid=1138676736#gid=1138676736"


# Read project sheet and clean
df_raw <- read_sheet(ss, sheet = "Projects_2")

df <- df_raw %>% janitor::clean_names()

# extract all collaborations and unique company names. Note that these names need to be harmonized with existing list

collaborations_raw <- df %>% 
  select(id, project_name, confirmed_partners) %>% 
  separate_rows(confirmed_partners, sep = ", ") %>%
  mutate(confirmed_partners = str_squish(confirmed_partners)) 

df_clean <- read_sheet(ss, sheet = "new_names")

collab_fixed <-collaborations_raw %>% 
  left_join(df_clean, by=c("confirmed_partners"="Old_name"))

write_sheet(collab_fixed, ss, sheet = "collaborations")

ref <-df %>% 
  select(id, project_name, contains("ref")) %>% 
  pivot_longer(contains("ref"), names_to = "ref", values_to = "url") %>%
  na.omit()
write_sheet(ref, ss, sheet = "references")


actors <- collab_fixed %>% select(New_name, Comment) %>% distinct()
write_sheet(actors, ss, sheet = "actors")
