# Split data frame into test and validation sets
load_test_and_train_data = function(df) {
  set.seed(1)
  train_indices = sample(1:nrow(df), 0.8 * nrow(df), replace = FALSE)

  train = df[train_indices,]
  validate = df[-train_indices,]

  return (list(train = train, validate = validate))
}

if (!exists("sentence_data")) {
  filename = "sentence_data.RData"
  if (file.exists(filename)) {
    load(file = filename)
  }
  else {
    source("data/data-preprocessing.R")
    sentence_data = load_test_and_train_data(inputData$sentences)
    save(sentence_data, file = filename)
  }
}

# Attempt to ensure records per key are equal in length by supersampling
balance_data = function(df, key_column = 1) {
  df #%>% group_by(.[[1]])
}

#balance_data(data$train)
