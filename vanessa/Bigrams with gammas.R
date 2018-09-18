

library(tidyverse)
library(dplyr)
library(tidytext)
library(topicmodels)


load("input_data.RData")

inputdata <- input_data$sentences


#Getting Bigrams out
tidy_sona3 = input_data$sentences %>%  unnest_tokens(bigram, sentence, token = "ngrams", n  = 2)
#class(tidy_sona3$id)

#Get stop words
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

#get counts of bigrams
bigrams_count <- bigrams_united %>%
  group_by(bigram, id ) %>%
  summarise(count_bigram = n()) %>%
  select(id, bigram, count_bigram)

  
#View(bigrams_count)

##Lets try and optimise
## I thought it might help to filter out but it actual makes things more complicated later on.
dtm_grams <- bigrams_count %>% 
#  filter(tf_idf >= 2) %>%
cast_dtm(id, bigram, count_bigram)

#loop for optimisations
print(Sys.time())
result <- FindTopicsNumber(
  dtm_grams,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
print(Sys.time())

FindTopicsNumber_plot(result)

##chose around 10 topics
set.seed(1234)
reviews_lda <- LDA(dtm_grams, k = 10, control = list(seed = 1234))


#for some reason, this is not working
#specch_topics <- tidy(reviews_lda, matrix = "beta")
#this works  rather
term <- as.character(reviews_lda@terms)
speech_topics <- as.tibble(cbind(term, t(reviews_lda@beta)))
colnames(speech_topics) <- c("Bigrams", "topic1","topic2","topic3","topic4","topic5","topic6","topic7","topic8","topic9","topic10")



#gathers into tidy format 
#note a term should appear more than once.
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
         , key = "topic", value = "beta")

speech_topics <- speech_topics %>%
  mutate(beta = as.numeric(beta)) %>%
  mutate(beta = exp(beta)) # pr(topic k generates word i) = exp(beta_ik)


#get the top terms for each topic
top_terms <- speech_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#plot
top_terms %>%
  mutate(Bigrams = reorder(Bigrams, beta)) %>%
  ggplot(aes(Bigrams, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

##lets look at terms without ctross over of terms

small_topic <- speech_topics %>%
  filter(topic == "topic1"|topic == "topic2")

beta_spread <- small_topic %>%
#  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(Bigrams = reorder(Bigrams, log_ratio)) %>%
  ggplot(aes(Bigrams, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()


##same for  1 and 10


small_topic <- speech_topics %>%
  filter(topic == "topic1"|topic == "topic10")


beta_spread <- small_topic %>%
  #  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic10 > .001) %>%
  mutate(log_ratio = log2(topic10 / topic1))

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(Bigrams = reorder(Bigrams, log_ratio)) %>%
  ggplot(aes(Bigrams, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 10 / topic 1") +
  coord_flip()

##need to do some work on classifying this


##############Gammas


##Get the gamma matrix
#gamma matrix gives an idication of the probability that each sentence belongs to one of the topics.
#the @documents is the sentence_ID

View(head(speech_documents))

speech_documents <- cbind(as.character(reviews_lda@documents), tidy(reviews_lda@gamma), stringsAsFactors = FALSE)
colnames(speech_documents) <- c("id","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")
speech_documents$X1 <- as.double(speech_documents$X1)
speech_documents$X1 <- as.double(speech_documents$X2)
speech_documents$X1 <- as.double(speech_documents$X3)
speech_documents$X1 <- as.double(speech_documents$X4)
speech_documents$X1 <- as.double(speech_documents$X5)
speech_documents$X1 <- as.double(speech_documents$X6)
speech_documents$X1 <- as.double(speech_documents$X7)
speech_documents$X1 <- as.double(speech_documents$X8)
speech_documents$X1 <- as.double(speech_documents$X9)
speech_documents$X1 <- as.double(speech_documents$X10)



#Just a quick test. The Topc 6 looks like itnroductons so it is working.
topic_matrix <- left_join(inputdata,  speech_documents, by = "id")

topic_matrix <- topic_matrix %>%
  mutate(X1 = ifelse(is.na(X1), 0, X1)) %>%
  mutate(X2 = ifelse(is.na(X1), 0, X2)) %>%
  mutate(X3 = ifelse(is.na(X1), 0, X3)) %>%
  mutate(X4 = ifelse(is.na(X1), 0, X4)) %>%
  mutate(X5 = ifelse(is.na(X1), 0, X5)) %>%
  mutate(X6 = ifelse(is.na(X1), 0, X6)) %>%
  mutate(X7 = ifelse(is.na(X1), 0, X7)) %>%
  mutate(X8 = ifelse(is.na(X1), 0, X8)) %>%
  mutate(X9 = ifelse(is.na(X1), 0, X9)) %>%
  mutate(X10 = ifelse(is.na(X1), 0, X10)) 
  

  
dim(topic_matrix)
#View(head(topic_matrix))
#save this for Merve
save(topic_matrix, file = "vanessa/topic_matrix.RData")

##I'm not sure why sentence 6 has just "NA"
#I think its because it was ahsort sentence and it got removed with the stopped words.
#View(bigrams_united)
#View(tidy_sona3)
#View(bigrams_filtered)
#tbl_vars(topic_matrix)

##########Geeting to the graphs

#View(head(topic_matrix))


##find the topic per sentence
data.max <- as.data.frame(topic_matrix[,6:15])
data.max <- apply(topic_matrix[,6:15],1,which.max) 
data.max[sapply(data.max, is.null)] <- NA
data.max <- unlist(as.numeric(as.character(data.max)))

topic_matrix <- cbind(topic_matrix, data.max)

View(head(topic_matrix))
##gather data
topic_matrix_long <- topic_matrix %>%
  select(id, president, sentence, X1,X2,X3,X4,X5,X6,X7,X8,X9,X10, data.max) %>%
  gather(X1, X2,X3,X4,X5,X6,X7,X8,X9,X10, key = "topic", value = "gamma")

#pivot
data.graph <- topic_matrix_long %>%
  group_by(president, data.max) %>%
  summarise(count_topic = n())
#grapgh
##Graph shows that sentences relating to topic 2 and 6 do not featrue in Klerk speeched
## Mandela mostly talks about topic 8
##Mbeki coveres the range of topics
#Zuma barly talks about 2
  data.graph %>%
    mutate(term = reorder(president, count_topic * data.max)) %>%
    ggplot(aes(y= count_topic, x= data.max,  fill = factor(data.max))) +
    geom_col(show.legend = FALSE) +
    scale_x_discrete(name ="Topics", 
                     limits=c("1","2","3","4","5","6","7","8","9","10")) +
    facet_wrap(~ president, scales = "free") 
  
  
  ##########
  data.graph <- topic_matrix_long %>%
    group_by(president, data.max) %>%
    summarise(count_topic = n())
  #grapgh
  ##Graph shows that sentences relating to topic 2 and 6 do not featrue in Klerk speeched
  ## Mandela mostly talks about topic 8
  ##Mbeki coveres the range of topics
  #Zuma barly talks about 2
  data.graph %>%
    mutate(term = reorder(president, count_topic * data.max)) %>%
    ggplot(aes(y= count_topic, x= data.max,  fill = factor(data.max))) +
    geom_col(show.legend = FALSE) +
    scale_x_discrete(name ="Topics", 
                     limits=c("1","2","3","4","5","6","7","8","9","10")) +
    facet_wrap(~ president, scales = "free") 