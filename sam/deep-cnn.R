source("sam/nn-setup.R")
#data = setup_nn(0.9, 10, 80, FALSE)

# Build model
max_features <- max_words  # choose max_features most popular words
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
    filters = 50,
    kernel_size = 5,
    padding = "valid",  # "valid" means no padding, as we did it already
    activation = "relu",
    strides = 1
  ) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(256) %>%
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
  layer_dense(64) %>%
  layer_dropout(0.5) %>%
  layer_activation("relu") %>%

  #layer_max_pooling() %>%
  layer_dense(64) %>%
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
