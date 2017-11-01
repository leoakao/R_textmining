# Yelp Reviews

place = 'tasteoftexas'
setwd("C:/Projects/R_textmining/Yelp")
reviews <- read.csv(paste("reviews",place,".csv",sep=""),stringsAsFactors = FALSE)


library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(reshape2)

# Parsing words in analysis-------------------------------------------------------------------------

# custom_stop_words to replace stop_words
custom_stop_words <- bind_rows(data_frame(word = c("miss"), 
                                          lexicon = c("custom")), 
                               stop_words)

tidyreviews <- reviews %>%
  mutate(rnum = row_number()) %>% 
  unnest_tokens(word, Text) %>% 
  anti_join(stop_words)

tidyreviews <- tidyreviews[is.na(as.numeric(tidyreviews$word)),]
subset(tidyreviews,is.na(tidyreviews$word)) 

# Top Words
tidyreviews %>%
  count(word, sort = TRUE) 

tidyreviews %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Sentimental Analysis (Comparing 3 Sentiment Lexicons)----------------------------------------------------------
# get_sentiments("afinn")
# get_sentiments("bing")
# get_sentiments("nrc")

yelp_af <- tidyreviews %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(rnum) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN") %>%
  ungroup()
  
yelp_b <- tidyreviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(rnum, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(method = "Bing et al.")
yelp_b$positive<-NULL
yelp_b$negative<-NULL

yelp_nrc <- tidyreviews %>% 
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(rnum, sentiment) %>%
  mutate(method = "NRC") %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
yelp_nrc$positive<-NULL
yelp_nrc$negative<-NULL

yelpsentiment = rbind(yelp_af, yelp_b, yelp_nrc)

# Plot Comparing 3 Sentiment Lexicons
ggplot(yelpsentiment, aes(rnum, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_x") +
  labs(y = "Sentiment",
       x = NULL) 

# Top 10 Positive and Negative Words Plot ------------------------------------------------
bing_word_counts <- tidyreviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Word Cloud------------------------------------------------------------------------------------
tidyreviews %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

maxnumwords = 100
# Colored Word Cloud
# By BING
tidyreviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = maxnumwords)

# By NRC
tidyreviews %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = maxnumwords)



