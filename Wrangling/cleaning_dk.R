
######################### Notes

# The script reads the danish adds (dk_hs.rds) and removes all(?) unwanted text, adds a new column with the cleaned text, and saves it as a new file (dk_short.rds). Please note that not all unwanted text is removed and also check so that not too much text is removed.  

##########################

library(tidyverse)
library(stringr)


dk_rds <- readRDS("~/OneDrive - Chalmers/HydrogenValleys/Shared_Data/dk_h2.rds") %>%
  unique() %>% rowid_to_column("iid")


remove_lines_with_pattern <- function(text, pattern) {
  # Pattern to match an entire line containing the specified pattern
  line_pattern <- paste0(".*", pattern, ".*\n?")
  str_replace_all(text, line_pattern, "")
}

pattern <-c("Cookiebot|HTTP|HTML|Thumbnails|Zoom|Cookie|cookie|ID|domains|consent|window|button|Expiry|SessionType:|\\{|\\}")


ddk <- dk_rds %>% 
  select(iid, all_text) %>%
  mutate(nr_breaks = str_count(all_text, pattern = "\n")) %>%
  mutate(nr_dots = str_count(all_text, pattern = "\\.")) %>%
  mutate(all_text_chr = str_length(all_text))

ddk_clean_breaks <- ddk %>%
  mutate(clean_breaks = map_chr(all_text, ~remove_lines_with_pattern(., pattern), .progress = T)) %>%
  mutate(clean_chr = str_length(clean_breaks))

df_view <- ddk_clean_breaks %>%
  arrange(desc(clean_chr)) %>% select(all_text, clean_breaks) %>% slice_head(n = 5)

DT::datatable(df_view) # look at the data to see if enough was removed (or too much)


df_short <- ddk_clean_breaks %>% select(iid, short_text = clean_breaks) %>% unique() 

dk_short <- dk_rds %>% 
  left_join(df_short) %>%
  mutate(all_text_chr = str_length(all_text),
         short_text_chr = str_length(short_text)) 

saveRDS(dk_short, file = "~/OneDrive - Chalmers/HydrogenValleys/Shared_Data/dk_short.rds")


