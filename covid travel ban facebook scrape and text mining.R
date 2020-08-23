# Webscraping with RSelenium and Text Mining of Facebook comments of the post of the 
# German government announcing travel bans and mandatory testing for travellers from spain

# In the analysis I am doing a webscrape with RSelenium, a Sentiment Analysis,
# analysis of bigrams, and topic modeling using LDA

library(RSelenium)
library(dplyr)
library(tm)
library(rvest)
library(SnowballC)
library(radiant.data)
library(tidytext)
library(stringr)
library(igraph)
library(ggraph)
library(topicmodels)

# Setting up Selenium
rsDriver(port = 1254L, browser ="firefox")
remDr <- remoteDriver$new(port = 1254L, browser = "firefox")
remDr$open()

# Navigating to the FB Post and clicking to the comments section
remDr$navigate("https://www.facebook.com/Bundesregierung/photos/a.769938079764597/3264453450313035/#")

webElem1 <- remDr$findElement("xpath", '//*[@id="expanding_cta_close_button"]')
webElem1$clickElement()

webElem2 <- remDr$findElement("xpath", '/html/body/div[1]/div[4]/div[1]/div/div[2]/div[2]/div[2]/div[2]/div/div/div/div/div/div/div/div[1]/div/div[2]/div[1]/div[3]/div[3]/div/div/div/div/a')
webElem2$clickElement()

# Expanding all comments. 
# These lines must be repeated until Selenium cant find the web element anymore
webElem3 <- remDr$findElement("class name", 'UFIPagerLink')
webElem3$clickElement()

# Opening all reply comments and showing all text 
webElem4 <- remDr$findElements("class name", 'UFICommentLink')

for(i in 1:length(webElem4)){
  webElem4[[i]]$clickElement()
}

webElem5 <- remDr$findElements("class name", '_5v47')

for(i in 1:length(webElem5)){
  webElem5[[i]]$clickElement()
}

# Saving webpage and retrieve comments and usernames
fb <- read_html(remDr$getPageSource()[[1]])

names <- fb %>% 
  html_nodes(".UFICommentActorName") %>% 
  html_text

comment <- fb %>% 
  html_nodes(".UFICommentBody") %>% 
  html_text

# Filtering out all comments by Bundesregierung
df <- data.frame(names, comment)

df_filtered <- df %>% 
  filter(names != "Bundesregierung")

### Sentiment Analysis ###

# Building corpus and preprocessing

comcorpus <- VCorpus(VectorSource(df_filtered$comment))
as.character(comcorpus[[54]])

comcorpus_clean <- tm_map(comcorpus, removeNumbers)
comcorpus_clean <- tm_map(comcorpus_clean, content_transformer(tolower))

as.character(comcorpus_clean[[54]])


com_dtm <- DocumentTermMatrix(comcorpus_clean, control = list(
                              removePunctuation = T))

sums <- as.data.frame(colSums(as.matrix(com_dtm)))
sums <- rownames_to_column(sums)
colnames(sums) <- c("words", "count")
sums <- arrange(sums, desc(count))
head(sums)

# Reading in SentWS dictionary for sentiment analysis
sent <- c(
  readLines("C:/Users/Julian/Documents/SentiWS_v2.0/SentiWS_v2.0_Positive.txt", 
            encoding = "UTF-8"),
  readLines("C:/Users/Julian/Documents/SentiWS_v2.0/SentiWS_v2.0_Negative.txt", 
            encoding = "UTF-8")
) %>% lapply(function(x){
  res <- strsplit(x, "\t", fixed = T)[[1]]
  return(data.frame(words = res[1], value = res[2], 
                    stringsAsFactors = F))
}) %>% 
  bind_rows() %>% 
  mutate(words = gsub("\\|.*", "", words) %>% tolower(),
         value = as.numeric(value)) %>% 
  group_by(words) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

# Joining the matrix with the dictionary
sentTxt <- left_join(sums, sent, by = "words") %>% 
  mutate(value = as.numeric(value),
         sent = ifelse(value >= 0, "positive", "negative")) %>% 
  filter(!is.na(value))

# Printing Overall score
mean(sentTxt$value)

# Plotting most frequent negative & positive words
sentTxt %>% 
  arrange(desc(count)) %>%  
  top_n(20, count) %>% 
  ggplot(aes(reorder(words, count), count, fill = sent)) +
    geom_col(show.legend = F) +
    facet_wrap(~sent, scales = "free_y") +
    labs(x = NULL, y = "Häufigkeit") +
    coord_flip()

### Bigram analysis ###

count_bigrams <- df_filtered[2] %>% 
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  filter(!bigram %in% tolower(names)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% new_stops,
         !word2 %in% new_stops) %>%
  count(word1, word2, sort = T) %>% 
  unite(bigram, c(word1, word2), sep = " ", remove = T)

ggplot(top_n(count_bigrams, 15), 
       aes(reorder(bigram, n), n)) +
  geom_col() +
  labs(x = NULL, y = "Häufigkeit") +
  coord_flip() +
  theme_minimal()

bigrams <- df_filtered[2] %>% 
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  filter(!bigram %in% tolower(names)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% new_stops,
         !word2 %in% new_stops) %>% 
  count(word1, word2, sort = T)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

bigrams %>% 
  filter(n > 2) %>% 
graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# individual bigrams 

indi_bigram_1 <- df_filtered[2] %>% 
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  filter(!bigram %in% tolower(names)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = T) %>% 
  filter(word1 == "nicht")
indi_bigram_2 <- df_filtered[2] %>% 
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  filter(!bigram %in% tolower(names)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  count(word1, word2, sort = T) %>% 
  filter(word2 == "nicht")

rbind(indi_bigram_1, indi_bigram_2) %>% 
  filter(n > 0) %>% 
graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


### Topic Modelling ###

comcorpus_clean <- tm_map(comcorpus, removeNumbers)
comcorpus_clean <- tm_map(comcorpus_clean, content_transformer(tolower))
comcorpus_clean <- tm_map(comcorpus_clean, removePunctuation)
comcorpus_clean <- tm_map(comcorpus_clean, removeWords, new_stops)
comcorpus_clean <- tm_map(comcorpus_clean, stemDocument, language = "german")


tm_dtm <- DocumentTermMatrix(comcorpus_clean)
tm_dtm <- removeSparseTerms(tm_dtm, 0.997)
labeled_terms <- as.data.frame(as.matrix(tm_dtm))
labeled_terms <- labeled_terms[rowSums(abs(labeled_terms)) != 0,]

# LDA Modelling

fb_lda <- LDA(labeled_terms, k = 2, control = list(seed = 10))

topics <- tidy(fb_lda, matrix = "beta")

top_terms <- topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>% 
  ggplot(aes(reorder(term, beta), beta, 
             fill = factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free") +
    coord_flip() +
    theme_bw()
