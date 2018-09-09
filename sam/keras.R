require(keras)
require(dplyr)
keras::use_python("D:/Dev/Anaconda3")
keras::use_condaenv("D:/Dev/Anaconda3")
reticulate::import_from_path("rpytools", path = "C:/Program Files/R/R-3.4.4/library/reticulate/python")

if (!exists("inputData"))
  source("sam/data-preprocessing.R")

#presidents = unlist(inputData$presidents)

#keras::text_one_hot(presidents, n = length(presidents))

#text = "The brown quick fox jumped over the lazy dog"

#keras::text_hashing_trick(text, n = 18, lower = TRUE, hash_function = "md5")

#wordData = inputData$words %>% select(president, word) %>% as.tibble()
#sentenceData = inputData$sentences %>% select(president, sentence) %>% as.tibble()

max_words = 8
tokenizer = keras::text_tokenizer(num_words = max_words)

tokenizer$fit_on_texts(c("hello world", "by SAM"))
tokenizer$texts_to_matrix(c("hello world", "by world", "hello hello by SAM"), mode = "binary")
tokenizer$texts_to_sequences(c("hello world", "by world", "hello hello by SAM"))
texts_to_sequences(tokenizer, c("hello world", "by world", "hello hello by by"))

sentences = inputData$sentences %>% select(sentence) %>%
  as_tibble() %>%
  dplyr::top_n(10,wt = "sentence") %>%
  unlist()

max_words = inputData$words %>% select(word) %>% unique() %>% count()
max_words = 30

tokenizer = keras::text_tokenizer(num_words = max_words)
tokenizer$fit_on_texts(sentences)
matrix = tokenizer$texts_to_matrix(sentences, mode = "binary") %>% dplyr::as_tibble()
tokenizer$texts_to_matrix(sentences, mode = "count") %>% dplyr::as_tibble()
matrix
