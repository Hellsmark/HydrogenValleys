# wrangling the h2_projects 

#### Libraries and reading data ======
library(tidyverse)
library(googlesheets4)

# project sheet
ss <- "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit?gid=1138676736#gid=1138676736"

# name conversion sheet from h2 competence book
ss_comp <- "https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit?gid=875872147#gid=875872147"


# Read project sheet and clean
df_raw <- read_sheet(ss, sheet = "Projects_2")

df <- df_raw %>% janitor::clean_names()

# extract all collaborations and unique company names. Note that these names need to be harmonized with existing list

collaborations_raw <- df %>% 
  select(id, confirmed_partners) %>% 
  separate_rows(confirmed_partners, sep = ", ") %>%
  mutate(confirmed_partners = str_squish(confirmed_partners)) 

companies_raw <- collaborations_raw %>% 
  select(confirmed_partners) %>%
  distinct()
  
# Read sheet with cleaned and harmonized company names
comp_names_raw <- read_sheet(ss_comp, sheet = "company_names")

# add new names to the list
updated_list_to_clean <- comp_names_raw %>% 
  full_join(companies_raw, by = c("Old_name"= "confirmed_partners"))

# write the new data to the old sheet and clean manually!
#write_sheet(updated_list_to_clean, ss_comp, sheet = "company_names")

# read cleaned names
comp_names_clean <- read_sheet(ss_comp, sheet = "company_names")

# only those names relevant for projects
actors_h2_projects <- comp_names_clean %>%
  filter(Old_name %in% companies_raw$confirmed_partners) %>%
  full_join(companies_raw, by = c("Old_name" = "confirmed_partners"))

write_sheet(actors_h2_projects, ss, sheet = "new_names")
# update collaboration with cleaned names. Many names missing. No point in making actor list yet

collaborations <- collaborations_raw %>% 
  left_join(comp_names_clean, by=c("confirmed_partners" = "Old_name")) %>%
  select(id, partners = New_name)

projects <- df %>% select(-confirmed_partners, -contains("ref"))

#upload new sheets
#write_sheet(projects, ss, sheet = "main_projects" )
#write_sheet(collaborations, ss, sheet = "collaborations" )
