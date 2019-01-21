

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stringr)
library(tidyr)
library(scales)
library(broom)
library(purrr)
library(widyr)
library(igraph)
library(ggraph)
library(SnowballC)
library(wordcloud)
library(reshape2)

# Load File

df <- read.csv2("YOUR PATH/"Vinos comentados.csv")

df <- df[complete.cases(df),]

# Transform variables

df$country <- NULL # All wines are Spanish

df$description <- as.character(df$description)

df$designation <- as.character(df$designation)

df$price <- as.integer(df$price)

df$title <- as.character(df$title)

# Common words

df<- tibble::rowid_to_column(df,"ID")

review_words <- df %>% 
  distinct(description, .keep_all = TRUE) %>%
  unnest_tokens(word,description,drop = FALSE) %>%
  distinct(ID,word,.keep_all = TRUE)%>%
  anti_join(stop_words,by="word")%>%
  group_by(word)%>%
  mutate(word_total=n()) %>%
  ungroup()

word_counts <- review_words %>%
  count(word,sort = TRUE)

word_counts %>%
  head(25) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n))+
  geom_col(fill="brown2")+
  scale_y_continuous(labels = comma_format())+
  coord_flip()+
  labs(title = "Palabras más usadas",
       subtitle = "De los 6.221 comentarios,conectores eliminados",
       y= "Nº Veces que aparece la palabra")

# Good results

# Group words by their stem

word_counts %>%
  head(25) %>%
  mutate(word= wordStem(word)) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col(fill="brown2")+
  scale_y_continuous(labels = comma_format())+
  coord_flip()+
  labs(title="Palabras más usadas",
       subtitle="De los 6.221 comentarios, conectores eliminados y agregadas por infimitivo",
       y= "Nº de veces que aparece la palabra")

# Which words appear together most often

review_bigrams <- df %>%
  unnest_tokens(bigram,description,token="ngrams",n=2)

bigrams_separated <- review_bigrams %>%
  separate(bigram,c("word1","word2"),sep=" ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_counts <- bigrams_filtered %>%
  count(word1,word2,sort = TRUE)

# Most used words: berry aromas y fruit flavours

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united %>%
  count(bigram, sort = TRUE)

review_subject <- df %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words)

my_stopwords <- tibble(word = c(as.character(1:10)))

review_subject <- review_subject %>% 
  anti_join(my_stopwords)

title_word_pairs <- review_subject %>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)

set.seed(1234)

title_word_pairs %>%
  filter(n >= 300) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "brown2") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Word network reviews vinos')
theme_void()

# Most common trigrams

review_trigrams <- df %>%
  unnest_tokens(trigram, description, token = "ngrams", n = 3)

trigrams_separated <- review_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")

trigrams_united %>%
  count(trigram, sort = TRUE)

#  Sentiment Analisis

reviews <- df %>% 
  filter(!is.na(description)) %>% 
  select(ID, description) %>% 
  group_by(row_number()) %>% 
  ungroup()

tidy_reviews <- reviews %>%
  unnest_tokens(word, description)

tidy_reviews <- tidy_reviews %>%
  anti_join(stop_words)

bing_word_counts <- tidy_reviews %>%
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
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() + 
  ggtitle('Words that contribute to positive and negative sentiment in the reviews')

# Try other libraries

contributions <- tidy_reviews %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))

contributions %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  ggtitle('Words with the greatest contributions to positive/negative 
          sentiment in reviews') +
  geom_col(show.legend = FALSE) +
  coord_flip()

# Let's introduce more context by using bigrams

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# Not convincing results

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  ggtitle('The 20 words preceded by "not" that had the greatest contribution to 
          sentiment scores, positive or negative direction') +
  coord_flip()

#Negation common words

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

negated_words %>%
  mutate(contribution = n * score,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  top_n(12, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment score * # of occurrences") +
  ggtitle('The most common positive or negative words to follow negations 
          such as "no", "not", "never" and "without"') +
  coord_flip()

# Analisis of most positive and negative comments

sentiment_messages <- tidy_reviews %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(ID) %>%
  summarize(sentiment = mean(score),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5)

sentiment_messages %>%
  arrange(desc(sentiment))

# Best comment:

df[ which(df$ID==2006), ]$description[1]

sentiment_messages %>%
  arrange(sentiment)

# Worst comment:

df[ which(df$ID==3975), ]$description[1]

