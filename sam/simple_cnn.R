require(keras, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidytext, quietly = TRUE)
keras::use_python("D:/Dev/Anaconda3")
keras::use_condaenv("D:/Dev/Anaconda3")
reticulate::import_from_path("rpytools", path = "C:/Program Files/R/R-3.4.4/library/reticulate/python")

source("sam/data-preprocessing.R", echo = FALSE)

# Calculate distinct number of words in corpus
max_words = input_data$words %>% select(word) %>% unique() %>% count()
# Calculate maximum word count in any sentence
max_length= input_data$words %>% group_by(id) %>% summarise(n = n())
min_length = 10
max_length = 80

max_words = 0.8 * max_words

# Filter out short sentences
sentence_data = inputData$sentences %>%
  filter(stringi::stri_length(sentence) > min_length) #%>% filter(president != "deKlerk")

# Extract sentences and presidents
sentences = sentence_data %>% select(sentence) %>% unlist()
presidents = sentence_data %>% select(president) %>% unlist()

# Convert sentences to matrix of input vectors
tokenizer = keras::text_tokenizer(num_words = max_words)
tokenizer$fit_on_texts(sentences)
sequences = tokenizer$texts_to_sequences(sentences)
x_data = pad_sequences(sequences, max_length, padding = "pre", truncating = "pre")

# One-hot encode president
president_count = presidents %>% as_tibble() %>% unique() %>% count()
response_tokenizer = text_tokenizer(num_words = president_count + 1)
response_tokenizer$fit_on_texts(presidents)
# Extract response vector, ignoring first (empty) column
y_data = (response_tokenizer$texts_to_matrix(presidents, mode = "binary"))[,-1]

# Create test and train sets
train_indices = sample(1:nrow(x_data), 0.8 * nrow(x_data), replace = FALSE)
x_train = x_data[train_indices,]
x_valid = x_data[-train_indices,]
y_train = y_data[train_indices,]
y_valid = y_data[-train_indices,]


# Build model
max_features <- max_words  # choose max_features most popular words
embedding_dims <- 50       # number of dimensions for word embedding

model = keras::keras_model_sequential()

model %>%
  # embedding layer maps vocab indices into embedding_dims dimensions
  layer_embedding(max_features, embedding_dims, input_length = max_length) %>%
  # add some dropout
  layer_dropout(0.5) %>%
  # convolutional layer
  layer_conv_1d(
    filters = 50,
    kernel_size = 5,
    padding = "valid",  # "valid" means no padding, as we did it already
    activation = "relu",
    strides = 1
  ) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(256) %>%
  layer_dropout(0.5) %>%
  layer_activation("relu") %>%
  layer_dense(president_count) %>%   # single unit output layer
  layer_activation("softmax")

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

model %>%
  fit(
    x_train, y_train,
    batch_size = 32,
    epochs = 10,
    validation_data = list(x_valid, y_valid)
  )
