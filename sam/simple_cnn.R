require(keras)
require(dplyr)
require(tidytext)
keras::use_python("D:/Dev/Anaconda3")
keras::use_condaenv("D:/Dev/Anaconda3")
reticulate::import_from_path("rpytools", path = "C:/Program Files/R/R-3.4.4/library/reticulate/python")

if (!exists("inputData"))
  source("sam/data-preprocessing.R", echo = FALSE)

max_words = inputData$words %>% select(word) %>% unique() %>% count()
max_length= inputData$words %>% group_by(id) %>% summarise(n = n())
#max_length %>% group_by(n) %>% summarise(y = n()) %>% ggplot(aes(x = n, y = y)) + geom_line() + geom_point()


tokenizer = keras::text_tokenizer(num_words = max_words)

sentence_data = inputData$sentences %>% filter

sentences = inputData$sentences %>% select(sentence) %>% unlist()

tokenizer$fit_on_texts(sentences)

sequences = tokenizer$texts_to_sequences(sentences)

padded_sequences = pad_sequences(sequences, 30, padding = "post", truncating = "post")

# One-hot encode president
presidents = inputData$presidents %>% unlist() %>% trimws()

response_tokenizer = text_tokenizer(num_words = length(presidents) + 1)
response_tokenizer$fit_on_texts(presidents)
response_tokenizer$texts_to_matrix(presidents, mode = "binary")
