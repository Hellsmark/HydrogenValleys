####Loading packages####
library(stm)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2) # For plotting word frequencies
library(ldatuning) 
library(googlesheets4)
library(textstem)
library(stopwords)

#### Loading Data ####

# Authorize googlesheets4 and googledrive to access your Google Sheets and Drive
# Please note that you need to have a Google account and be signed in to it in your web browser for it to work
#drive_auth()
gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

# Get the Google Sheet by its name within the shared folder
sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1xzpre5Ej_7OEGRU4EA7KZuMQnSz5YCyTx5Sdbml6bQE/edit#gid=0")

main <- read_sheet(sheet, "Main")

se_ads <- main %>% filter(ID < 20000 &  !is.na(Description)) 


#### Preprocessing ####

# Get the list of Swedish stopwords
comp_stopwords <- c('vattenfall', 'siemens','ovako','afry','jefferson','wells','boden', 'vattenfalls')
extra_stopwords <- c('mer', ' nya', 'projekt', 'genom', 'anställningen', 'får', 'tror', 'gärna', 'arbet','tillsammans', 'möjlighet', 'utveckla', 'utveckling', 'kollegor', 'kunskap',
                     'vätgas','del','arbete','svenska', 'engelska','person','sveriges','välkommen','olika','arbet','kunskaper','arbetsuppgifter','företag','kontakta', 'tal','god','goda',
                     'förmåga','löpande','stora','stor','fokus','sker','skrift','möjligheter','fram','bidra','innebär','via','därför','flera','verksamhet','frågor','kompetens','ansvar',
                     'lösningar','många','självständigt','såväl','också','relevant','team','anställda','gender','arbetet','mångfald', 'sökande')

swedish_stopwords <- c(stopwords("sv"), comp_stopwords, extra_stopwords,c('kommer', 'både', 'samt', 'söker', 'arbeta', 'även', 'ansökan', 'ska', 'arbetar', 'vill', 'erfarenhet','tjänsten','hos',
                                                           'ser','medarbetare','rollen','andra','uppdrag'))
#### counting words ####
tidy_se_ads <- se_ads %>% unnest_tokens(word, Description) %>% filter(!(word %in% swedish_stopwords)) 
word_counts <- tidy_se_ads %>%
  group_by(word) %>%
  summarise(n_ads = n_distinct(ID))

# Preprocess the text data
processed <- textProcessor(documents = se_ads$Description, metadata = se_ads, 
                           customstopwords = swedish_stopwords, stem = FALSE)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

####Finding words to remove####
# Flatten the documents list into a single vector
all_words <- unlist(lapply(out$documents, function(x) out$vocab[x[1,]]))

# Calculate the frequency of each word
word_freq <- sort(table(all_words), decreasing = TRUE)

# Convert the sorted words to a data frame
sorted_words_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)

# Get the list of unique words
unique_words <- names(word_freq)

# Create a binary matrix
binary_matrix <- lapply(out$documents, function(x) as.integer(unique_words %in% out$vocab[x]))

# Convert the list to a matrix
binary_matrix <- do.call(rbind, binary_matrix)

# Calculate the number of unique documents each word occurs in
unique_docs <- colSums(binary_matrix)

# Convert the sorted words and unique documents count to a data frame
sorted_words_df <- as.data.frame(cbind(Frequency = word_freq, UniqueDocuments = unique_docs), stringsAsFactors = FALSE)

# Select the top 20 words
top_20_words <- head(sorted_words_df, 20)

# Print the top 20 words
print(top_20_words)









#### STM model ####
# Fit the STM model 
stm_model <- stm(documents = out$documents, vocab = out$vocab, K = 12)

# Print the topics
print(stm_model)

# Plot the topics
plot(stm_model)

findThoughts(stm_model,texts = se_ads$Company, n = 20, topics = c(10,11))

#### choosing K ####
#link to the page I've been using
#https://journals.sagepub.com/doi/10.1177/25152459231160105

findk <- searchK(out$documents, out$vocab, K = c(20:40))

findk$results %>% 
  pivot_longer(cols = -K, names_to = "metric", values_to = "value") %>%
  filter(metric %in% c('lbound', 'exclus', 'residual', 'semcoh')) %>%
  mutate(K = unlist(K),
         value = unlist(value)) %>%
  mutate(value = map_dbl(value, 1)) %>% 
  mutate(K = map_dbl(K,1)) %>%
  ggplot(aes(x = K, y = value, color = metric)) +
  geom_point() + geom_line() + guides(color = "none") +
  facet_wrap(~ metric, scales = "free")

#exlus = Exclusivity represents the degree to which words are exclusive to a single topic rather than associated with multiple topics
#lbound = Variational lower bound is the metric used to determine convergence for a specific solution
#residual = Residual is the estimation of the dispersion of residuals for a particular solution
#semcoh = semantic coherence is a measure of how commonly the most probable words in a topic co-occur

#### counting the number of ads in each topic ####
# Get the topic proportions for each document
topic_proportions <- stm_model$theta

# Find the topic with the highest proportion for each document
max_topic <- apply(topic_proportions, 1, which.max)

# Count the number of documents associated with each topic
doc_counts <- table(max_topic)

print(doc_counts)
