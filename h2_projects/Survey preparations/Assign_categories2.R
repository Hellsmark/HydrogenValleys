library(ellmer)
library(tidyverse)
library(tidyverse)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(stringr)

OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

ss <- "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit#gid=1219310046"

# Read project sheet and clean
df_raw <- read_sheet(ss, sheet = "contacts_categories")
df <- df_raw %>% janitor::clean_names()
small_df <- df%>% select(org,eight_categories)

# Kod för att assigna kategorier till företag som inte har fått en kategori i contacts formuläret

#Jag vill att koden ska gå igenom alla kontakter OM categorin är tom
# OM den är det, använd API för att assigna en kategori basseard på företagsnamnet

# Outputstruktur: 1 kolumn "Company category"
output_structure <- type_array(
  items = type_object(
    category = type_string("Company category")  # Ändrat från "name" till "category" för tydlighet
  )
)

results <- list()

for (i in 600:908) {
  if (is.na(small_df$eight_categories[i])) {
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
    
    extracted_data <- chat$extract_data(
      small_df$org[i],
      type = output_structure
    )
    
    result <- tibble(
      org = small_df$org[i],
      category = extracted_data$category[1]
    )
    
    results[[i]] <- result
    print(paste("Processed", i, "-", small_df$org[i]))
  }
}

# Slutlig resultat-tabell
fin_df <- bind_rows(results)

fin_df<- fin_df%>% distinct()


df_cleaned <- fin_df %>%
  # Gruppera på aktör
  group_by(org) %>%
  # Välj första förekomsten (eller valfri logik) och lägg till *
  slice(1) %>%
  ungroup()



new_df<- df %>% left_join(df_cleaned,by="org") %>%
  mutate(eight_categories = if_else(is.na(eight_categories),category,eight_categories))%>%
  select(-category)

write_sheet(new_df,ss,sheet="contacts_categories")


df1<- read_sheet(ss,sheet = "contacts_categories")

df%>%
  count(eight_categories, sort=TRUE)

df<- df%>% distinct(full_name,.keep_all=TRUE)

