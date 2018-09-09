library(topicmodels)

count_words <- count_words %>%
  group_by(sentence_id,word) %>%
  count() %>%  
  ungroup() 



dtm_words <- count_words %>% 
  cast_dtm(sentence_id, word, n)


set.seed(1234)
#reviews_lda <- LDA(dtm_words, k = 5)
reviews_lda <- LDA(dtm_words, k = 2)
#reviews_lda


#str(reviews_lda)


term <- as.character(reviews_lda@terms)
topic1 <- reviews_lda@beta[1,]
topic2 <- reviews_lda@beta[2,]
# topic3 <- reviews_lda@beta[3,]
# topic4 <- reviews_lda@beta[4,]
# topic5 <- reviews_lda@beta[5,]
speech_topics <- tibble(term = term, topic1 = topic1, topic2 = topic2)
                        #, topic3= topic3, topic4 = topic4, topic5 = topic5)


speech_topics <- speech_topics %>% 
  gather(topic1, topic2, topic3, topic4, topic5, key = "topic", value = "beta") %>%
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