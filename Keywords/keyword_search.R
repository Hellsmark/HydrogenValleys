library(tidytext)
library(ggplot2)
library(forcats)
library(dplyr)
ams_h2 <- ams %>% filter(`s_terms[i]` == "vätgas*")
#calculating the frequency of the words used in the ads
words <- ams_h2 %>% unnest_tokens(word,description) %>% count(id,word, sort = TRUE)

#Calculating the total number of words used by companies in their ads 
total_words <- words %>% group_by(id) %>% summarize(total = sum(n))

#joining to one tibble
words <- as_tibble(left_join(words, total_words))

#plotting the term frequency (n/total) of four chosen companies
words %>% 
  filter(id %in% c("27494670","27550665", "26938909","26876107")) %>%
  ggplot(aes(n/total, fill = id)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~id, ncol = 2, scales = "free_y")

#adding tank and term frequency
freq_by_rank <- words %>% 
  group_by(id) %>% 
  mutate(rank = row_number(), `term frequency` = n/total) %>%
  ungroup()

#plotting rank on the x-axis and term frequency on the y-axis (Zipf's law)
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = id)) + 
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#calculating the exponent of the power law for the middle section
rank_subset <- freq_by_rank %>% filter(rank < 100, rank > 10)
freq_by_rank %>% ggplot(aes(rank, `term frequency`, color = company)) + 
  geom_abline(intercept = -1.05, slope = -0.83, color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#calculating the tf-idf and arranging the table so that the words with high tf-idf
#are at the top
ams_tf_idf <- words %>%
  bind_tf_idf(word, id, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf))


#plotting the words with the highest tf-idf for 4 companies
company_tf_idf %>%
  filter(company %in% c("Siemens Energy AB", "Vattenfall AB", "AFRY AB", "Experis AB")) %>%
  group_by(company) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = company)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~company, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
  
