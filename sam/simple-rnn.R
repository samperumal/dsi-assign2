source("sam/nn-setup.R")
#model = keras::keras_model_sequential(name = "RNN")

# Parameters --------------------------------------------------------------

# Embedding
max_features = 20000
maxlen = 120
embedding_size = 50

data = setup_nn(1.0, 8, maxlen, FALSE)

# Convolution
kernel_size = 5
filters = 256
pool_size = 3

# LSTM
lstm_output_size = 70

# Training
batch_size = 32
epochs = 10

# Data Preparation --------------------------------------------------------

# The x data includes integer sequences, each integer is a word
# The y data includes a set of integer labels (0 or 1)
# The num_words argument indicates that only the max_fetures most frequent
# words will be integerized. All other will be ignored.
# See help(dataset_imdb)
#imdb <- dataset_imdb(num_words = max_features)
# Keras load all data into a list with the following structure:
#str(imdb)

# Pad the sequences to the same length
# This will convert our dataset into a matrix: each line is a review
# and each column a word on the sequence
# We pad the sequences with 0s to the left
#x_train <- imdb$train$x %>%
#  pad_sequences(maxlen = maxlen)
#x_test <- imdb$test$x %>%
#  pad_sequences(maxlen = maxlen)

# Defining Model ------------------------------------------------------

model <- keras_model_sequential()

model %>%
  layer_lstm(units = 32, return_sequences = TRUE, stateful = TRUE) %>%
  layer_lstm(units = 32, return_sequences = TRUE, stateful = TRUE) %>%
  layer_lstm(units = 32, stateful = TRUE) %>%
  layer_dense(units = 6, activation = 'softmax')

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

# Training ----------------------------------------------------------------

model %>% fit(
  data$train$x, data$train$y,
  batch_size = batch_size,
  epochs = epochs,
  validation_data = list(data$validate$x, data$validate$y)
)

#model = model_attempt
predictions = model %>% predict_classes(data$validate$x) + 1
actuals = apply(data$validate$y, 1, which.max)
comparison = cbind(predictions, actuals, isequal = ifelse(predictions == actuals, 1, 0))

#View(comparison[comparison[,1] == comparison[,2],])

sum(comparison[,3]) / nrow(comparison)

