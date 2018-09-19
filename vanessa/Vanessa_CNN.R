
##########Librabrys
require(keras, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidytext, quietly = TRUE)

#####LoadData
load("input_data.RData")
load("balanced_train_data.RData")
load("sentence_data.RData")


#######SAM - this is where I get confused!!
### Calculate distinct number of words in corpus
max_words = input_data$words %>% select(word) %>% unique() %>% count()

#why are we only taking 90% of words?
max_words = 0.9 * max_words # Only take specified percentage of words
#max_words
#max_words = 10657 or 9591.3
# Calculate maximum word count in any sentence
max_length= input_data$words %>% group_by(id) %>% summarise(n = n()) %>% arrange(desc(n))
#max_length
#max_length = 119
min_length= input_data$words %>% group_by(id) %>% summarise(n = n()) %>% arrange((n))
#min_length
#min_length =  1


#why when this is different?
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
#Also Sam - not sure but it seems to drop 4 lines here.
train = tokenize_data(balanced_train_data)
validate = tokenize_data(sentence_data$validate)


#######Get the validation data out
#not sure why but the model fit didn't like the "$" reference
val_x <- validate$x
val_y <- validate$y


############################
#FEATURES OF NN
##########################
#Sam - this is also where I get confused

max_features <- 10657
maxlen <- 80
batch_size <- 32
embedding_dims <- 70
filters <- 250
kernel_size <- 3
hidden_dims <- 250
epochs <- 10

#min_length = 7
#max_length = 80
#max_words = 10657
#max_length = 119
#min_length =  1
#max_features <- max_words  # choose max_features most popular words
#embedding_dims <- 70       # number of dimensions for word embedding

###############################
##Model!!
################################
model_attempt <- keras_model_sequential()

model_attempt %>% 
  # Start off with an efficient embedding layer which maps
  # the vocab indices into embedding_dims dimensions
  layer_embedding(max_features, embedding_dims, input_length = maxlen) %>%
  layer_dropout(0.2) %>%
  
  # Add a Convolution1D, which will learn filters
  # Word group filters of size filter_length:
  layer_conv_1d(
    filters, kernel_size, 
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  # Apply max pooling:
  layer_global_max_pooling_1d() %>%
  
  # Add a vanilla hidden layer:
  layer_dense(hidden_dims) %>%
  
  # Apply 20% layer dropout
  layer_dropout(0.2) %>%
  layer_activation("relu") %>%
  
  # Project onto a 6 unit output layer, and squash it with a sigmoid
  
  layer_dense(president_count) %>%
  layer_activation("sigmoid")

# Compile model
model_attempt %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

# Training ----------------------------------------------------------------

model_attempt %>%
  fit(
    train$x, train$y,
    batch_size = batch_size,
    epochs = epochs,
    validation_data = list(val_x, val_y)
  )


######################################
#Sumamry of model
######################################
model_attempt

########Look at export
##This is just me trying to see if its actucally predicting

predictions <- model_attempt %>% predict(val_x)


#Getting the max of each rows prediction and binding it on the end
pres.max <- as.data.frame(predictions)
pres.max <- apply(predictions,1,which.max) 
pres.max <- unlist(as.numeric(as.character(pres.max)))
pres.max <- cbind(predictions,pres.max )


pres.actual <- as.data.frame(val_y)
pres.actual <- apply(val_y,1,which.max) 


predictions_val <- cbind(pres.max,pres.actual)
head(predictions_val)
##I can't connect back to sentecne as the tokeniser drops 4 lines?

