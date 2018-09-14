

library(tidyverse)
library(dplyr)
library(tidytext)
library(topicmodels)

#read data in
txt_files <- list.files("sona-text-1994-2018/")

#need to delete that .M file for this to work.
#loop to read in file
sona <- data.frame(filename = as.character(), speech = as.character())
for(i in txt_files){
  file_name <- paste0("sona-text-1994-2018/", i)
  
  # import text as single character string (can also read.table but the "seperator" causes problems)
  this_speech <- readChar(file_name, 
                          nchars = file.info(file_name)$size)
  
  # make data frame with metadata (filename contains year and pres) and speech
  this_sona <- data.frame(filename = i, speech = this_speech, stringsAsFactors = FALSE)
  
  # make a single dataset
  sona <- rbind(sona, this_sona)
}


#get year out
str_sub(sona$filename, start = 1, end = 4)
sona$year <- str_sub(sona$filename, start = 1, end = 4)


#get ptesident name out
sona$president <- unlist(str_extract_all(sona$filename, "[A-Z]+[a-z]+"))

# word tokenization
sona %>% unnest_tokens(text, speech, token = "words")

# we want to predict sentences, so we need to first split into sentences
tidy_sona <- sona %>% unnest_tokens(text, speech, token = "sentences")



#id varaibele for eah sentence
tidy_sona = tibble::rowid_to_column(tidy_sona, "ID")
colnames(tidy_sona)[1] <- "sentence_id"

#Getting Bigrams out
tidy_sona3 = tidy_sona %>%  unnest_tokens(bigram, text, token = "ngrams", n  = 2)

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
  group_by(bigram,sentence_id ) %>%
  summarise(count_bigram = n()) %>%
  select(sentence_id, bigram, count_bigram)


#lets see if it works with the tfidf
#it doesn't, btw. So this is jut hre for fun
bigrams_tfidf <- bigrams_count %>%
  bind_tf_idf(bigram, sentence_id, count_bigram) %>%
  arrange( desc(tf_idf))

##distribution of tdif
plot(bigrams_tfidf$tf_idf)

  


##Lets try and optimise
## I thought it might help to filter out but it actual makes things more complicated later on.
dtm_grams <- bigrams_tfidf %>% 
#  filter(tf_idf >= 2) %>%
cast_dtm(sentence_id, bigram, count_bigram)

#loop for optimisations
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

speech_documents <- cbind(as.numeric(reviews_lda@documents), tidy(reviews_lda@gamma))
colnames(speech_documents) <- c("sentence_id","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")
#dim(speech_documents)
#View(head(speech_documents))


#Just a quick test. The Topc 6 looks like itnroductons so it is working.
topic_matrix <- left_join(tidy_sona,  speech_documents, by = "sentence_id")
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


##gather
topic_matrix_long <- topic_matrix %>%
  select(sentence_id, president, text, X1,X2,X3,X4,X5,X6,X7,X8,X9,X10) %>%
  gather(X1, X2,X3,X4,X5,X6,X7,X8,X9,X10, key = "topic", value = "gamma")


#View(head(topic_matrix))


##find the topic per sentence
data.max <- as.data.frame(topic_matrix[,6:15])
data.max <- apply(topic_matrix[,6:15],1,which.max) 
data.max[sapply(data.max, is.null)] <- NA
data.max <- unlist(as.numeric(as.character(data.max)))

topic_matrix <- cbind(topic_matrix, data.max)


##gather data
topic_matrix_long <- topic_matrix %>%
  select(sentence_id, president, text, X1,X2,X3,X4,X5,X6,X7,X8,X9,X10, data.max) %>%
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
  