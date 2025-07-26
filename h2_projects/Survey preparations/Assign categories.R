library(ellmer)
library(tidyverse)
library(googlesheets4)
library(janitor)
library(dplyr)
library(stringr)

ss <- "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit#gid=1219310046"

df_raw <- read_sheet(ss, sheet = "actors")
df <- df_raw %>% janitor::clean_names()

# Outputstruktur: 1 kolumn "Company category"
output_structure <- type_array(
  items = type_object(
    category = type_string("Company category")  # Ändrat från "name" till "category" för tydlighet
  )
)

results <- list()

N <- 459  # Byt till nrow(df) när du vill köra hela listan

for (i in 1:N) {
  chat <- chat_openai(
    system_prompt = "You are assisting in classifying organizations involved in hydrogen projects in the Nordic countries.
    Each organization should be assigned **one primary category** based on its role in the hydrogen economy. Choose the best fitting category based on their activities, offerings, or purpose. If the fit is unclear or if the organization spans multiple categories, choose the **most relevant one** and mark it with an asterisk (*).
    
    Use the following categories:
      
    1. **Producer** – Develops, invests in, or operates hydrogen production (including synthetic fuels based on hydrogen).
    2. **User / End user** – Uses hydrogen in its own processes or operations (e.g., heavy industry, transport).
    3. **Technology & System Supplier** – Provides technologies or systems such as electrolyzers, compressors, refueling stations, etc.
    4. **Public actor / Authority** – Influences projects through planning, regulation, subsidies or strategic direction (e.g., national agencies, municipalities, EU bodies).
    5. **Research & Academia** – Contributes knowledge, technology development, or policy analysis.
    6. **Infrastructure / Network Operator** – Manages electricity or gas grids, pipelines, ports, or related infrastructure.
    7. **Industry Association / Cluster** – Coordinates networks, members or projects related to hydrogen.
    8. **Financier / Investor** – Provides funding to hydrogen-related projects or companies.

    Output the result in a structured JSON format like this:
      ```json
    {
      'name': 'Company name',
      'category': 'Producer'
    }
    ")
  
  Sys.sleep(1)
  
  input_text <- paste("Company name:", df$new_name[i],
                      "\nAdditional info:", df$info[i])
    
  extracted_data <- chat$extract_data(
    input_text,
    type = output_structure)
  
  # Lägg till företagsnamn i resultatet
  result <- tibble(
  new_name = df$new_name[i],
  category = extracted_data$category[1]
  )
  results[[i]] <- result
  print(paste("Processed", i, "-", df$new_name[i]))
}

# Slutlig resultat-tabell: Företag till vänster, kategori till höger
final_df <- bind_rows(results)

write_sheet(final_df, ss, sheet="company_category")




# The following code is to try out different types of categorisations of the actors

df_raw <- read_sheet(ss, sheet = "company_category")
df <- df_raw %>% janitor::clean_names()

five_cat <- df %>% mutate(five_categories= if_else(category=="Financier / Investor","Support & Capital actors",
                                            if_else(category=="Industry Association / Cluster", "Support & Capital actors",
                                            if_else(category=="Technology & System Supplier", "Technology & Infrastructure",
                                            if_else(category=="Infrastructure / Network Operator", "Technology & Infrastructure",
                                            if_else(category=="Producer", "Producer & User",
                                            if_else(category=="User / End user", "Producer & User",category)))))))

four_cat <- five_cat %>% mutate(four_categories =if_else(category=="Research & Academia", "Knowledge actors & Coordination",
                                                  if_else(category=="Industry Association / Cluster", "Knowledge actors & Coordination",
                                                  if_else(category=="Financier / Investor", "Market Actors",
                                                  if_else(category=="Infrastructure / Network Operator", "Technology & Infrastructure",
                                                  if_else(category=="Technology & System Supplier", "Technology & Infrastructure",
                                                  if_else(category=="Producer", "Market Actors",
                                                  if_else(category=="User / End user", "Market Actors",category))))))))

# Edit and then count the actors in our data


categories <- four_cat %>%
  rename(eight_categories = category, actor=new_name)

categories%>%
  count(four_categories, sort=TRUE)

categories%>%
  count(five_categories, sort=TRUE)

categories%>%
  count(eight_categories, sort=TRUE)

write_sheet(categories, ss, sheet="company_category")



















#The following can be used to extract the contacts that we have in our contact file


#df_a <- read_sheet(ss,sheet="missing_actors")
#df_a<- df_a%>%janitor::clean_names() 
#flt<- df_a %>% filter(str_detect(tolower(match),"yes"))

#df_raw <- read_sheet(ss, sheet = "actors")
#df <- df_raw %>% clean_names()

#df<- df %>% filter(new_name %in% flt$company)
