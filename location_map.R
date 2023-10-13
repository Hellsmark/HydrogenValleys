library(data.table)
library(dplyr)
library(tidyverse)
library(csmaps)
library(ggtext)
library(ggmap)
library(ggplot2)
library(maps)
library(rworldmap)
library(ggbump)
library(ggforce)
library(ggtext)



###----------------------------------------------------------------------------###


#Collect our jobb-adds from csv and filter out location information
main <- read.csv("/Users/viktorrosenberg/Documents/Jobb/Chalmers/WP3-H2_Job_advertisements - Main.csv")

ad_info <- main %>% select(company,ID,location) %>% filter(ID > 0,location != "", location != "Obestämd ort", location != "#N/A")

ad_loc <- ad_info$location

# choose cities in relevant copuntries
cities <- world.cities %>% filter(country.etc %in% c("Sweden", "Norway", "Denmark")) %>% select(!pop) %>% select(!capital)

# change letters and names of cities so that it matches english vocabulary
ad_loc <- gsub("å","a",ad_loc)
ad_loc <- gsub("ä","a", ad_loc)
ad_loc <- gsub("ö","o", ad_loc)
ad_loc <- gsub("Å","A", ad_loc)
ad_loc <- gsub("Ä","A", ad_loc)
ad_loc <- gsub("Ö","O", ad_loc)
ad_loc <- gsub("æ","ae", ad_loc)
ad_loc <- gsub("Stavanger","Stavanger-Sandnes", ad_loc)
ad_loc <- gsub("Alborg","Aalborg", ad_loc)
ad_loc <- gsub("Alestrup","Aalestrup", ad_loc)
ad_loc <- gsub("Porsgrunn","Porsgrunn-Skien", ad_loc)
ad_loc <- gsub("Kopenhamn","Copenhagen", ad_loc)

# do a check over what cities that we have documented without pre-set geographical information
ad_loc <- data.frame(ad_loc)

missing <- anti_join(ad_loc, cities, join_by(ad_loc == name))

# convertion of citydistricts into cities they are part of
ad_loc <- gsub("Solna","Stockholm", ad_loc$ad_loc)
ad_loc <- gsub("Lillestrøm","Oslo", ad_loc)
ad_loc <- gsub("Lysaker","Oslo", ad_loc)
ad_loc <- gsub("Kokstad","Bergen", ad_loc)
ad_loc <- gsub("Skjetten","Oslo", ad_loc)
ad_loc <- gsub("Sandvika","Oslo", ad_loc)
ad_loc <- gsub("Kjeller","Oslo", ad_loc)
ad_loc <- gsub("Sandsli","Bergen", ad_loc)
ad_loc <- gsub("Nesttun","Bergen", ad_loc)
ad_loc <- gsub("Lyngby","Copenhagen", ad_loc)
ad_loc <- gsub("Ballerup","Copenhagen", ad_loc)
ad_loc <- gsub("Hvidovre","Copenhagen", ad_loc)
ad_loc <- gsub("Brabrand","Arhus", ad_loc)
ad_loc <- gsub("Risskov","Arhus", ad_loc)
ad_loc <- gsub("Naerum","Copenhagen", ad_loc)

ad_info$location <- ad_loc

# manual adding of geographical information
missing_dist <- distinct(missing)

Lat_for_missing <- c(67.13,59.36,57.47,60.55,55.37,
                     62.53,58.06,59.71,66.05,57.03,
                     58.72,59.96,59.91,60.30,
                     59.97,59.88,59.98,59.84,59.80,
                     60.29,60.32,55.78,55.73,
                     55.87,55.64,56.15,55.82,
                     56.18)
Lon_for_missing <- c(20.66,18.00,18.49,16.29,13.18,
                      15.66,11.84,16.23,17.88,16.45,
                      11.32,11.05,10.64,5.27,
                     11.00,10.52,11.03,10.44,5.47,
                     5.28,5.35,12.50,12.37,
                     12.37,12.48,10.12,12.55,
                     10.23)

countries_for_missing <- c("Sweden","Sweden","Sweden","Sweden","Sweden",
                           "Sweden","Sweden","Sweden","Sweden","Sweden",
                           "Sweden","Norway","Norway","Norway",
                           "Norway","Norway","Norway","Norway","Norway",
                           "Norway","Norway","Denmark","Denmark",
                           "Denmark","Denmark","Denmark","Denmark",
                           "Denmark")

missing_found <- missing_dist %>% mutate(country.etc = countries_for_missing, lat = Lat_for_missing, long = Lon_for_missing)
colnames(missing_found)[colnames(missing_found) == 'ad_loc'] <- 'name'

cities <- rbind(cities, missing_found)

# creating new datafram of all our cities and their information needed for plot
ad_info <- inner_join(ad_info,cities,join_by(location == name)) 

ads_pos_unique <- ad_info %>% count(location)

ads_pos <- ad_info %>% select(location,country.etc,lat,long)

ads_pos_dist <-  distinct(ads_pos)

ads_pos_final <- full_join(ads_pos_unique,ads_pos_dist) 


###----------------------------------------------------------------------------###



# setting up the map
se_dk_no <- map_data("world", region = c("Norway(?!:Svalbard)(?!:Jan Mayen)","Sweden","Denmark"))

map1 <- ggplot(data=se_dk_no, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = region), color="white")+
  coord_fixed(1.8) + 
  theme_void() +
  scale_fill_manual(values=c("Sweden" = "darkolivegreen3", "Denmark" = "cornflowerblue", "Norway" = "khaki1"))
#map1
map2 <- map1 +
  geom_point(data = ads_pos_final, aes(x = long, y = lat, group = 1), 
             colour = "Red", fill = "Pink", pch = 21,  size = ads_pos_final$n^(1/2), alpha = 0.7) + 
  theme(plot.margin = margin(m, m, m, m, "cm"),
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    plot.caption = element_text(color = "gray40"),
    text = element_text(family = "Helvetica"),
    plot.title = element_text(color = "gray40", size = 16, family = "Helvetica", face = "bold"),
    plot.subtitle = element_text(color = "gray40", size = 8))
#map2


###----------------------------------------------------------------------------###


### Här skapar jag 8 regioner
se_north <- ads_pos_final %>% filter(lat > 62, long > 14.4) %>% filter(lat < 68) %>% mutate(region = "Northern Sweden")

se_east <- ads_pos_final %>% filter(lat > 56.5, long > 15) %>% filter(lat < 61,long < 20) %>% mutate(region = "Eastern Sweden")

se_west <- ads_pos_final %>% filter(lat > 56.8, long > 11) %>% filter(lat < 59.7,long < 14.3) %>% mutate(region = "Western Sweden")

no_west <- ads_pos_final %>% filter(lat > 57.5, long > 5) %>% filter(lat < 61,long < 7) %>% mutate(region = "Western Norway")

no_east <- ads_pos_final %>% filter(lat > 57.5, long > 7) %>% filter(lat < 61,long < 11) %>% mutate(region = "Eastern Norway")

no_north1 <- ads_pos_final %>% filter(lat > 63.3, long > 10.2) %>% filter(lat < 67,long < 14.4)
no_north2 <- ads_pos_final %>% filter(lat > 68.1, long > 14.5)
no_north <- rbind(no_north1, no_north2) %>% mutate(region = "Northern Norway")

dk_se <- ads_pos_final %>% filter(lat > 55, long > 11.5) %>% filter(lat < 56.5,long < 14) %>% mutate(region = "Southern Sweden/Eastern Denmark")

dk_west <- ads_pos_final %>% filter(lat > 55, long > 8) %>% filter(lat < 57.5,long < 11) %>% mutate(region = "Western Denmark")

final_test <- rbind(se_north,se_east,se_west,no_west,no_east,no_north,dk_se,dk_west) 



##plockar ut topp 5 företag i varje region samt antalet nya jobb företaget skapat 

se_north_top <- ad_info %>% filter(location %in% se_north$location & company != "") %>% count(company, sort = TRUE) %>% top_n(5) 
se_east_top <- ad_info %>% filter(location %in% se_east$location & company != "") %>% count(company, sort = TRUE) %>% top_n(5)
se_west_top <- ad_info %>% filter(location %in% se_west$location & company != "") %>% count(company, sort = TRUE) %>% top_n(5)
no_west_top <- ad_info %>% filter(location %in% no_west$location & company != "") %>% count(company, sort = TRUE) %>% top_n(5)
no_east_top <- ad_info %>% filter(location %in% no_east$location & company != "") %>% count(company, sort = TRUE) %>% top_n(5)
no_north_top <- ad_info %>% filter(location %in% no_north$location & company != "") %>% count(company, sort = TRUE) %>% top_n(5)
dk_se_top <- ad_info %>% filter(location %in% dk_se$location & company != "") %>% count(company, sort = TRUE) %>% top_n(5)
dk_west_top <- ad_info %>% filter(location %in% dk_west$location & company != "") %>% count(company, sort = TRUE) %>% top_n(5)


###----------------------------------------------------------------------------###


map3 <-map2 + 
  ggforce::geom_mark_hull(data = final_test,aes(group = region, fill = region), con.colour  = "white",colour = "white",expand=.02)
#map3


map4 <- map3 + geom_text(data = final_test, aes(long, lat,  label = n,group = 4),size = final_test$n^(1/3),color = "white")
#map4


###----------------------------------------------------------------------------###


## Define a function to generate an S-curve
generate_s_curve <- function(x1, y1, x2, y2, n_points = 100) {
  x <- seq(x1, x2, length.out = n_points)
  if (x1 < x2) {
    y <- y1 + (y2 - y1) / (1 + exp(-x + mean(c(x1, x2))))
  } else {
    y <- y2 + (y1 - y2) / (1 + exp(-x + mean(c(x1, x2))))
  }
  data.frame(x = x, y = y)
}


#Order -> se-north,east,west,dk-se,dk-west,no-east,west,north
lat_start <- c(65,59,58,55.81,
               56.3,59.1,59.7,68)
long_start <- c(18,17,12.7,12.7,
                9.8,9.4,5.45,15)

lat_end <- c(68.3,64,60,55,
             56,59.9,65.5,70)
long_end <- c(25.5,22,26,18,
              5,3,2.5,7)

se_north_region <- data.frame(region = "Northern Sweden",n = sum(se_north$n))
se_east_region <- data.frame(region = "Eastern Sweden",n = sum(se_east$n))
se_west_region <- data.frame(region = "Western Sweden",n = sum(se_west$n))
dk_se_region <- data.frame(region = "Øresund region",n = sum(dk_se$n))
dk_west_region <- data.frame(region = "Mainland Denmark",n = sum(dk_west$n))
no_east_region <- data.frame(region = "Eastern Norway",n = sum(no_east$n))
no_west_region <- data.frame(region = "Western Norway",n = sum(no_west$n))
no_north_region <- data.frame(region = "Northern Norway",n = sum(no_north$n))

all_regions <- rbind(se_north_region,se_east_region,se_west_region,dk_se_region,dk_west_region,no_east_region,no_west_region,no_north_region)
all_tops <- c(se_north_top,se_east_top,se_west_top,dk_se_top,dk_west_top,no_east_top,no_west_top,no_north_top)

#Add lines and region top 
map5 <- map4
#map5
for (i in seq(length(lat_end))) {
  s_curve <- generate_s_curve(long_start[i], lat_start[i], long_end[i], lat_end[i])
  map5 <- map5 + geom_path(data = s_curve, aes(x = x, y = y, group = 5), color = "pink")
  companies <- all_tops[-1+2*i]$company
  n <- all_tops[2*i]$n
  y_coords <- c()
  for (k in seq(length(companies))) {
    y_coords <- c(y_coords,lat_end[i]-k*.5)
  }
  if (i < 5) {
    map5 <- map5 + annotate("text", x = long_end[i]+.1, y = lat_end[i], label = paste(all_regions$region[i],all_regions$n[i],sep = " "), colour = "white", hjust = 0)
    for (j in seq(y_coords)) {
      map5 <- map5 + annotate("text", x = long_end[i]+.6, y = y_coords[j], label = paste(companies[j],n[j], sep = " "), colour = "white", hjust = 0)
    }
  } else {
    map5 <- map5 + annotate("text", x = long_end[i]-nchar(all_regions$region[i])*.5, y = lat_end[i], label = paste(all_regions$region[i],all_regions$n[i],sep = " "), colour = "white", hjust = 0)
    for (j in seq(y_coords)) {
      map5 <- map5 + annotate("text", x = long_end[i]-nchar(all_regions$region[i])*.5+.5, y = y_coords[j], label = paste(companies[j],n[j], sep = " "), colour = "white", hjust = 0)
    }
  }
} 
map5


