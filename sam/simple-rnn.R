source("sam/nn-setup.R")
data = setup_nn(0.9, 10, 80, FALSE)

model = keras::keras_model_sequential(name = "RNN")

model %>%
  layer_conv_1d(
