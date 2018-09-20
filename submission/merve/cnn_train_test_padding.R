# Number of words to consider as features
# Calculate distinct number of words in corpus
max_words = inputData$words %>% select(word) %>% unique() %>% count()
# Calculate maximum word count in any sentence
max_length= inputData$words %>% group_by(id) %>% summarise(n = n())
maxlen = max(max_length$n)


# Extract sentences and presidents
sentences = inputData$sentences %>% select(sentence) %>% unlist()
presidents = inputData$sentences %>% select(president) %>% unlist()


# Convert sentences to matrix of input vectors
tokenizer = keras::text_tokenizer(num_words = max_words)
tokenizer$fit_on_texts(sentences)
sequences = tokenizer$texts_to_sequences(sentences)
x_data = pad_sequences(sequences, max_length, padding = "pre", truncating = "post")

# Convert train to matrix of input vectors
tokenizer = keras::text_tokenizer(num_words = max_words)
x_train = sentence_data$train %>% select(sentence) %>% unlist()
tokenizer$fit_on_texts(x_train)
x_train = tokenizer$texts_to_sequences(x_train)

# Convert train to matrix of input vectors
tokenizer = keras::text_tokenizer(num_words = max_words)
x_test = sentence_data$validate %>% select(sentence) %>% unlist()
tokenizer$fit_on_texts(x_test)
x_test = tokenizer$texts_to_sequences(x_test)

# One-hot encode president
president_count = presidents %>% as_tibble() %>% unique() %>% count()
response_tokenizer = text_tokenizer(num_words = president_count + 1)
y_train = sentence_data$train %>% select(president) %>% unlist()
y_test = sentence_data$validate %>% select(president) %>% unlist()
response_tokenizer$fit_on_texts(y_train)
response_tokenizer$fit_on_texts(y_test)
# Extract response vector, ignoring first (empty) column
y_train = (response_tokenizer$texts_to_matrix(y_train, mode = "binary"))[,-1]
y_test = (response_tokenizer$texts_to_matrix(y_test, mode = "binary"))[,-1]


# This turns our lists of integers
# into a 2D integer tensor of shape `(samples, maxlen)`
x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test <- pad_sequences(x_test, maxlen = maxlen)