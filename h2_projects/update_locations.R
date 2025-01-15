library(tidyverse)
library(googlesheets4)
library(ggmap)




key <- Sys.getenv("GOOGLE_KEY")

register_google(key)

get_coordinates <- function(place_name) {
  result <- tryCatch({
    geocode(place_name, source = "google")
  }, error = function(e) {
    return(data.frame(lon = NA, lat = NA))
  })
  Sys.sleep(0.5) 
  return(result)
}

#testing
location <- get_coordinates("Ånge, Sweden")
print(location)


ss <-  "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit?gid=1138676736#gid=1138676736"

df <- read_sheet(ss, sheet = "main_projects")
unique(df$country)
loc <- df %>% select(location, c = country) %>%
  mutate(country = case_when(
    c == "DNK" ~ "Denmark",
    c == "FIN" ~ "Finland",
    c == "NOR" ~ "Norway",
    c == "SWE" ~ "Sweden"
  )) %>%
  distinct() %>%
  mutate(geo = map(paste(location, country, sep = ", "), get_coordinates)) 

df_loc <- loc %>%
  unnest(geo) %>%
  rename(country_code =c)

write_sheet(df_loc, ss, sheet = "locations")
