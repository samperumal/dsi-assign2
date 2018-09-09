library(tidyverse)
library(tidytext)

txt_files <- list.files("data/")

View(txt_files)

sona <- data.frame(filename = as.character(), speech = as.character())
for(i in txt_files){
  file_name <- paste0("data/", i)
  
  # import text as single character string (can also read.table but the "seperator" causes problems)
  this_speech <- readChar(file_name, 
                          nchars = file.info(file_name)$size)
  
  # make data frame with metadata (filename contains year and pres) and speech
  this_sona <- data.frame(filename = i, speech = this_speech, stringsAsFactors = FALSE)
  
  # make a single dataset
  sona <- rbind(sona, this_sona)
}

# extract year, testing
str_extract(sona$filename, "[0-9]")
str_extract_all(sona$filename, "[0-90-90-90-9]")
str_extract_all(sona$filename, "[0-9][0-9][0-9][0-9]")
str_extract_all(sona$filename, "[0-9][0-9][0-9][0-9]", simplify = T)
str_extract(sona$filename, "[0-9]{4}")

# does the same thing
str_sub(sona$filename, start = 1, end = 4)

sona$year <- str_sub(sona$filename, start = 1, end = 4)

# exercise: extract president name


presidents <- rbind(unlist(str_extract_all(sona$filename, "[A-Z]+[a-z]+")))
presidents

# pre-processing data to get into right format for neural nets

library(tidytext)

# unnest_tokens is very useful, can split into words, sentences, lines, paragraphs, etc

# word tokenization
sona %>% unnest_tokens(text, speech, token = "words")

# we want to predict sentences, so we need to first split into sentences
tidy_sona <- sona %>% unnest_tokens(text, speech, token = "sentences")
View(tidy_sona)

# exercise: add an ID variable for sentences and tokenize each sentence by words
#id varaibele 


tidy_sona = tibble::rowid_to_column(tidy_sona, "ID")
    colnames(tidy_sona)[1] <- "sentence_id"
    
    View(tidy_sona)
    
# exercise: count how many times each word was used in each sentence

number_word_sen <- sapply(strsplit(tidy_sona$text, " "), length)
tidy_sona <- cbind(tidy_sona,number_word_sen) 
    
    
       
count_words <- tidy_sona %>%
  unnest_tokens(word, text, token = "words") %>%
  group_by(word,sentence_id ) %>%
  summarise(count_word = n()) %>%
  select(sentence_id, word, count_word)

View(count_words)

tidy_sona_idf <- count_words %>%
  bind_tf_idf(word, sentence_id, count_word)


# exercise: reshape long to wide to get into usual format for predictive models 
# using "spread"

words_wide <- tidy_sona_idf %>% 
  select(sentence_id, word, count_word) %>%
  spread(key = "word", value = "count_word")



View(words_wide)

