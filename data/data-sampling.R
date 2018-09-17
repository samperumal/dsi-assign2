require(dplyr)

# Split data frame into test and validation sets
load_test_and_train_data = function(df) {
  set.seed(1)
  train_indices = sample(1:nrow(df), 0.8 * nrow(df), replace = FALSE)

  train = df[train_indices,]
  validate = df[-train_indices,]

  return (list(train = train, validate = validate))
}

if (!exists("sentence_data")) {
  sentence_data_filename = "sentence_data.RData"
  if (file.exists(sentence_data_filename)) {
    load(file = sentence_data_filename)
  }
  else {
    source("data/data-preprocessing.R")
    sentence_data = load_test_and_train_data(input_data$sentences)
    save(sentence_data, file = sentence_data_filename)
  }
}
