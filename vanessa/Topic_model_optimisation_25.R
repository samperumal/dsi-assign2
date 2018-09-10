install.packages("ldatuning")
library(ldatuning)
library(tidyverse)
library(dplyr)
library(topicmodels)



count_words <- tidy_sona2 %>%
  group_by(word,sentence_id ) %>%
  summarise(count_word = n()) %>%
  select(sentence_id, word, count_word)

data(stop_words)
count_words <- count_words %>% anti_join(stop_words,by=c("word"="word"))


dtm_words <- count_words %>% 
  cast_dtm(sentence_id, word, count_word)

print(Sys.time())
result <- FindTopicsNumber(
  dtm_words,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
print(Sys.time())

FindTopicsNumber_plot(result)


#k = 25



set.seed(1234)
reviews_lda <- LDA(dtm_words, k = 25)
#reviews_lda <- LDA(dtm_words, k = 2)
reviews_lda


str(reviews_lda)


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
topic11 <- reviews_lda@beta[11,]
topic12 <- reviews_lda@beta[12,]
topic13 <- reviews_lda@beta[13,]
topic14 <- reviews_lda@beta[14,]
topic15 <- reviews_lda@beta[15,]
topic16 <- reviews_lda@beta[16,]
topic17 <- reviews_lda@beta[17,]
topic18 <- reviews_lda@beta[18,]
topic19 <- reviews_lda@beta[19,]
topic20 <- reviews_lda@beta[20,]
topic21 <- reviews_lda@beta[21,]
topic22 <- reviews_lda@beta[22,]
topic23 <- reviews_lda@beta[23,]
topic24 <- reviews_lda@beta[24,]
topic25 <- reviews_lda@beta[25,]

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
                        , topic11 = topic11
                        , topic12 = topic12
                        , topic13 = topic13
                        , topic14 = topic14
                        , topic15 = topic15
                        , topic16 = topic16
                        , topic17 = topic17
                        , topic18 = topic18
                        , topic19 = topic19
                        , topic20 = topic20
                        , topic21 = topic21
                        , topic22 = topic22
                        , topic23 = topic23
                        , topic24 = topic24
                        , topic25 = topic25)
                        
                        

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
         , topic11
         , topic12
         , topic13
         , topic14
         , topic15,
         , topic16,
         , topic17
         , topic18
         , topic19
         , topic20
         , topic21
         , topic22
         , topic23
         , topic24,
         ,topic25 
         ,key = "topic", value = "beta") %>%
  mutate(beta = exp(beta)) # pr(topic k generates word i) = exp(beta_ik)
head(speech_topics)

speech_topics <- tidy(reviews_lda, matrix = "beta")
head(speech_topics)

top_terms <- speech_topics %>%
  group_by(topic) %>%
  top_n(40, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

View(top_terms)


beta_spread <- speech_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()