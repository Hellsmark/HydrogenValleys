#### Packages ####

library(tidyverse)
library(ggmap)
library(googlesheets4)


#### Function for getting the coordinates ####

# the function which when provided a string of a location name or a list of locations will return a table of longitudes and latitudes corrisponding to the locations provided
get_coordinates <- function(place_name) {
  result <- tryCatch({
    geocode(place_name, source = "google")
  }, error = function(e) {
    return(data.frame(lon = NA, lat = NA))
  })
  Sys.sleep(0.5) 
  return(result)
}

#### Google access ####

# Get access to sheet where we will store our information
gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")

#### Loading the locations which we lack coordinates for ####

locations <- read_sheet(sheet, "locations_coord")

missing_coord <- locations[is.na(locations$Longitude) | is.na(locations$Latitude), "Name2"]

#### Use our function to get the missing coordinates. !!! IMPORTANT !!! --> ggmap may fail to find some of the locations, for these may the country have to be specified and then added manually ###

new_coord <- get_coordinates(missing_coord[["Name2"]])

new_locations <- cbind(missing_coord,new_coord)

colnames(new_locations) <- c("Name2","Longitude", "Latitude")

#### Add new coordinates to existing dataframe and upload to google sheet ####

locations <- locations %>%
  left_join(new_locations, by = "Name2") %>%
  mutate(
    Longitude = coalesce(Longitude.x, Longitude.y),
    Latitude = coalesce(Latitude.x, Latitude.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

sheet_write(locations, ss = sheet, sheet = "locations_coord")

#### Other notes ####

#testing
#location <- get_coordinates(c("stockholm","oslo"))
#print(location)



#df2 <- data.frame(loc=c("stockholm","oslo")) #%>% rbind(location)

#df2 <- cbind(df2,location)

#loc <- c("Stockholm","oslo","copenhagen")
#lat <- c(1.5,NA,2.3)
#long <- c(3.2,NA,NA)

#other <- c("q","w","e")

#df <- data.frame(loc,lat,long,other)

#missing_locations <- df[is.na(df$long) | is.na(df$lat), "loc"]

#df_new <- data.frame(loc = missing_locations,lat = c(1.2,2.3),long = c(2.2,2.6))

#df[df$loc %in% df_new$loc, ] <- df_new


#df <- df %>%
#  left_join(df_new, by = "loc") %>%
#  mutate(
#    long = coalesce(long.x, long.y),
#    lat = coalesce(lat.x, lat.y)
#  ) %>%
#  select(-ends_with(".x"), -ends_with(".y"))
