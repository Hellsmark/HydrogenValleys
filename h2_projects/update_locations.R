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
location <- get_coordinates("North/central Jutland")
print(location)


ss <-  "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit?gid=1138676736#gid=1138676736"

df <- read_sheet(ss, sheet = "Projects_2", range = "A:O") %>% janitor::clean_names()
old_loc <- read_sheet(ss, sheet = "locations") %>% janitor::clean_names()

ddf <- df %>% 
  filter(!location %in% old_loc$location) %>% 
  filter(!location == "N/A")
  

unique(ddf$country)
new_loc <- ddf %>% select(location, c = country) %>%
  mutate(country = case_when(
    c == "DNK" ~ "Denmark",
    c == "FIN" ~ "Finland",
    c == "NOR" ~ "Norway",
    c == "SWE" ~ "Sweden", 
    c == "ISL" ~ "Iceland"
  )) %>%
  distinct() %>%
  mutate(geo = map(paste(location, country, sep = ", "), get_coordinates)) 

df_new_loc <- new_loc %>%
  unnest(geo) %>%
  rename(country_code =c)

df_loc <- bind_rows(old_loc, df_new_loc)

write_sheet(df_loc, ss, sheet = "locations")
