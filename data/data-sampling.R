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
  filename = "sentence_data.RData"
  if (file.exists(filename)) {
    load(file = filename)
  }
  else {
    source("data/data-preprocessing.R")
    sentence_data = load_test_and_train_data(input_data$sentences)
    save(sentence_data, file = filename)
  }
}

# Attempt to ensure records per key are equal in length by supersampling
balance_data = function(df, key_column = 1) {
  sentence_counts = df %>% group_by(.[[key_column]]) %>% summarise(key = .[[key_column]], n = n()) %>%
    select(key, n)

  max_count = sentence_counts %>% max(n)

  # Split data frame by key
  data_groups = df %>%
    group_by(.[[key_column]]) %>%
    do(data = (.)) %>%
    select(data) %>%
    map(identity)

  for (df in data_groups) {

  }

  return (max_sentences)
}


#dlply(df, "V1", identity)
#balance_data(sentence_data$train)

#key_column = "president"

#sentence_data$train %>% group_by(.[[key_column]]) %>% summarise(!!var, n = n())
