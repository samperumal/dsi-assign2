source("sam/nn-setup.R")

# Embedding
#max_features = 20000
max_length = maxlen = 100
embedding_size = 50

data = setup_nn(1.0, 2, max_length, FALSE)

# Build model
max_features <- 20000  # choose max_features most popular words
embedding_dims <- 50       # number of dimensions for word embedding

model = keras::keras_model_sequential(name = "Deep CNN")

model %>%
  # embedding layer maps vocab indices into embedding_dims dimensions
  layer_embedding(max_features, embedding_dims, input_length = max_length) %>%
  # add some dropout
  layer_dropout(0.5) %>%
  # convolutional layer
  layer_conv_1d(
    name = "conv1",
    filters = 250,
    kernel_size = 5,
    padding = "valid",  # "valid" means no padding, as we did it already
    activation = "relu",
    strides = 1
  ) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(256, kernel_regularizer = regularizer_l2()) %>%
  layer_dropout(0.5) %>%
  layer_activation("relu")

model %>%
  # convolutional layer 2
  #layer_conv_1d(
  #  name = "conv2",
  #  filters = 50,
  #  kernel_size = 5,
  #  padding = "valid",  # "valid" means no padding, as we did it already
  #  activation = "relu",
  #  strides = 1
  #) %>%
  #layer_global_max_pooling_1d() %>%
  layer_dense(64, kernel_regularizer = regularizer_l2()) %>%
  layer_dropout(0.5) %>%
  layer_activation("relu") %>%

  #layer_max_pooling() %>%
  layer_dense(64, kernel_regularizer = regularizer_l2()) %>%
  layer_dropout(0.5) %>%
  layer_activation("relu") %>%

  layer_dense(6) %>%   # single unit output layer
  layer_activation("softmax")

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

model %>% fit(
  data$train$x, data$train$y,
  batch_size = batch_size,
  epochs = epochs,
  validation_data = list(data$validate$x, data$validate$y),
  verbose = 1
)

predictions = model %>% predict_classes(data$validate$x) + 1
actuals = apply(data$validate$y, 1, which.max)
#comparison = cbind(predictions, actuals, isequal = ifelse(predictions == actuals, 1, 0))

#View(comparison[comparison[,1] == comparison[,2],])

sum(comparison[,3]) / nrow(comparison)
