#' Setup sequence data inputs for nn processing
#'
#' @param max_words_factor Percentage of most common words to use in sentence dictionary
#' @param min_length Shortest words to include in training and validation sets
#' @param max_length Longest words to include in training and validation sets
#' @param use_balanced_data Flag to indicate whether to supersample the training set to account for unbalanced data
#'
#' @return list(train, validate)
#' @export
#'
#' @examples setup_nn(0.9, 10, 80, TRUE)
setup_nn = function(max_words_factor, min_length, max_length, use_balanced_data = FALSE) {
  suppressPackageStartupMessages({
    require(dplyr, quietly = TRUE)
    require(tidytext, quietly = TRUE)
    require(keras, quietly = FALSE)
    require(tensorflow, quietly = FALSE)
    #reticulate::import_from_path("rpytools", path = "C:/Program Files/R/R-3.4.4/library/reticulate/python")
  })

  #tensorflow::tf$ConfigProto(intra_op_parallelism_threads=as.integer(3), inter_op_parallelism_threads=as.integer(3))
  source("data/data-balance-training.R", echo = FALSE)


  # Calculate distinct number of words in corpus
  max_words = input_data$words %>% select(word) %>% unique() %>% count()
  max_words = max_words_factor * max_words # Only take specified percentage of words

  # Calculate maximum word count in any sentence
  if (is.null(max_length))
    max_length= input_data$words %>% group_by(id) %>% summarise(n = n())
  #min_length = 10
  #max_length = 60

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
    x_data = pad_sequences(sequences, max_length, padding = "post", truncating = "post")

    # One-hot encode president
    # Extract response vector, ignoring first (empty) column
    y_data = (response_tokenizer$texts_to_matrix(filtered_list$president, mode = "binary"))[,-1]

    return (list(x = x_data, y = y_data))
  }

  # Transform test and validation sets
  if (!use_balanced_data) {
    train = tokenize_data(sentence_data$train %>% filter(stringi::stri_length(sentence) > min_length))
  }
  else {
    train = tokenize_data(
              balance_data(sentence_data$train %>%
                 filter(stringi::stri_length(sentence) > min_length),
                 "president")
            )
  }

  validate = tokenize_data(sentence_data$validate %>% filter(stringi::stri_length(sentence) > min_length))

  return (list(
    train = train,
    validate = validate
  ))
}
