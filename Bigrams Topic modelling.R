?unnest_tokens

library(ldatuning)
library(tidyverse)
library(dplyr)
library(tidytext)
library(topicmodels)

#Taking out regular expressions and toeknise

tidy_sona3 = tidy_sona %>%  unnest_tokens(bigram, text, token = "ngrams", n  = 2)

View(tidy_sona3)

data("stop_words")

# separate the bigrams 
tidy_sona3_sep <- tidy_sona3 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# remove stop words
bigrams_filtered <- tidy_sona3_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# join up the bigrams again
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_count <- bigrams_united %>%
  group_by(bigram,sentence_id ) %>%
  summarise(count_bigram = n()) %>%
  select(sentence_id, bigram, count_bigram)

View(bigrams_count)



dtm_grams <- bigrams_count %>% 
  cast_dtm(sentence_id, bigram, count_bigram)



print(Sys.time())
result <- FindTopicsNumber(
  dtm_grams,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
print(Sys.time())

FindTopicsNumber_plot(result)


set.seed(1234)
reviews_lda <- LDA(dtm_grams, k = 10)
#reviews_lda <- LDA(dtm_words, k = 2)
View(reviews_lda@wordassignments)
names(reviews_lda)

str(reviews_lda)

View(reviews_lda)

term <- as.character(reviews_lda@terms)
topic1 <- reviews_lda@beta[1,]
topic2 <- reviews_lda@beta[2,]
topic3 <- reviews_lda@beta[3,]
topic4 <- reviews_lda@beta[4,]
topic5 <- reviews_lda@beta[5,]
topic6 <- reviews_lda@beta[6,]
topic7 <- reviews_lda@beta[7,]
topic8 <- reviews_lda@beta[8,]
topic9 <- reviews_lda@beta[9,]
topic10 <- reviews_lda@beta[10,]


speech_topics <- tibble(term = term
                        , topic1 = topic1
                        , topic2 = topic2
                        , topic3= topic3
                        , topic4 = topic4
                        , topic5 = topic5
                        , topic6 = topic6
                        , topic7 = topic7
                        , topic8 = topic8
                        , topic9 = topic9
                        , topic10 = topic10
                        )

View(speech_topics)

speech_topics <- speech_topics %>% 
  gather(topic1
         , topic2
         , topic3
         , topic4
         , topic5
         , topic6
         , topic7
         , topic8
         , topic9
         , topic10
         , key = "topic", value = "beta") %>%
  mutate(beta = exp(beta)) # pr(topic k generates word i) = exp(beta_ik)
head(speech_topics)

View(reviews_lda@beta)

speech_topics <- tidy(reviews_lda, matrix = "beta")
View(top_terms)


top_terms <- speech_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

View(beta_spread)


beta_spread <- speech_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))


beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()