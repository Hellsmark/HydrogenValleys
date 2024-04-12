library(tidytext)
library(ggplot2)
library(forcats)
library(dplyr)

#calculating the frequency of the words used in the ads
ad_words <- ams %>% unnest_tokens(word,description) %>% count(`s_terms[i]`,word, sort = TRUE)

#Calculating the total number of words used by companies in their ads 
total_words <- ad_words %>% group_by(`s_terms[i]`) %>% summarize(total = sum(n))

#joining to one tibble
ad_words <- as_tibble(left_join(ad_words, total_words))

#calculating the tf-idf and arranging the table so that the words with high tf-idf
#are at the top
ad_tf_idf <- ad_words %>%
  bind_tf_idf(word, `s_terms[i]`, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#plotting the words with the highest tf-idf for h2 ads
ad_tf_idf %>%
  filter(`s_terms[i]` %in% c("vätgas*","hydrogen","vindkraft*","solenerg*")) %>%
  group_by(`s_terms[i]`) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = `s_terms[i]`)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~`s_terms[i]`, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


h2_words <- ad_tf_idf %>% filter(`s_terms[i]` == "vätgas*")
