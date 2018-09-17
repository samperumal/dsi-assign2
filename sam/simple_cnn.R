require(keras, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidytext, quietly = TRUE)
keras::use_python("D:/Dev/Anaconda3")
keras::use_condaenv("D:/Dev/Anaconda3")
reticulate::import_from_path("rpytools", path = "C:/Program Files/R/R-3.4.4/library/reticulate/python")
tensorflow::tf$ConfigProto(intra_op_parallelism_threads=as.integer(3), inter_op_parallelism_threads=as.integer(3))

source("data/data-preprocessing.R", echo = FALSE)
source("data/data-sampling.R", echo = FALSE)

# Calculate distinct number of words in corpus
max_words = input_data$words %>% select(word) %>% unique() %>% count()
max_words = 0.9 * max_words # Only take specified percentage of words

# Calculate maximum word count in any sentence
max_length= input_data$words %>% group_by(id) %>% summarise(n = n())
min_length = 7
max_length = 80

# Extract sentences and presidents
sentences = input_data$sentences %>% select(sentence) %>% unlist()
presidents = input_data$presidents %>% unlist()

# Fit input tokenizer
tokenizer = keras::text_tokenizer(num_words = max_words)
tokenizer$fit_on_texts(sentences)

# Fit president tokenizer
president_count = presidents %>% as_tibble() %>% unique() %>% count()
response_tokenizer = text_tokenizer(num_words = president_count + 1)
response_tokenizer$fit_on_texts(presidents)

tokenize_data = function(data_list) {
  # Filter out short sentences
  filtered_list = (data_list %>% filter(stringi::stri_length(sentence) > min_length))

  # Convert sentences to matrix of input vectors
  sequences = tokenizer$texts_to_sequences(filtered_list$sentence)
  x_data = pad_sequences(sequences, max_length, padding = "pre", truncating = "pre")

  # One-hot encode president
  # Extract response vector, ignoring first (empty) column
  y_data = (response_tokenizer$texts_to_matrix(filtered_list$president, mode = "binary"))[,-1]

  return (list(x = x_data, y = y_data))
}

# Transform test and validation sets
train = tokenize_data(sentence_data$train)
validate = tokenize_data(sentence_data$validate)

# Build model
max_features <- max_words  # choose max_features most popular words
embedding_dims <- 70       # number of dimensions for word embedding

model = keras::keras_model_sequential()

model %>%
  # embedding layer maps vocab indices into embedding_dims dimensions
  layer_embedding(max_features, embedding_dims, input_length = max_length) %>%
  # add some dropout
  layer_dropout(0.5) %>%
  # convolutional layer
  layer_conv_1d(
    filters = 50,
    kernel_size = 3,
    padding = "valid",  # "valid" means no padding, as we did it already
    activation = "relu",
    strides = 1
  ) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(128) %>%
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
    train$x, train$y,
    batch_size = 32,
    epochs = 10,
    validation_data = list(validate$x, validate$y)
  )
