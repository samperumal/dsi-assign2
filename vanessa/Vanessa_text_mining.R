library(tidyverse)
library(tidytext)


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
View(tidy_sona)


#Taking out regular expressions and toeknise
unnest_reg <- "[^A-Za-z\\d_#@']"
tidy_sona2 = tidy_sona %>%  unnest_tokens(word, text, token = "regex", pattern = unnest_reg)
View(tidy_sona2)

## tokensie words and count the number of words
count_words <- tidy_sona2 %>%
  group_by(word,sentence_id ) %>%
  summarise(count_word = n()) %>%
  select(sentence_id, word, count_word)
nrow(count_words)
View(count_words)


#Removing the stop words
data(stop_words)
count_words <- count_words %>% anti_join(stop_words,by=c("word"="word"))

nrow(count_words)


#get tfidf

tidy_sona_idf <- count_words %>%
  bind_tf_idf(word, sentence_id, count_word)


#spread the tfidf into wide format

words_wide <- tidy_sona_idf %>% 
  select(sentence_id, word, count_word) %>%
  spread(key = "word", value = "count_word")



View(head(words_wide, n=  5))

