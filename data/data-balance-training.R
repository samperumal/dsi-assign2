# Attempt to ensure records per key are equal in count by supersampling
balance_data = function(df, key_column) {
  # Split dataframe by specified key column
  key_groups = df %>%
    group_by(key = .[[key_column]])

  # Summarise number of sentences per key
  sentence_counts = key_groups %>%
    summarise(n = n()) %>%
    select(key = key, n = n)

  # Calculate maximum number of sentences present for a single key
  max_count = sentence_counts %>% summarise(max(n)) %>% unlist()

  # Split data frame by key
  data_groups = key_groups %>%
    do(data = (.)) %>%
    select(data) %>%
    map(identity)

  # Determine super samples across all keys
  set.seed(1)
  super_sampled = lapply(data_groups[[1]], FUN = function(df) {

    # Number of rows to generate
    to_generate = max_count - nrow(df)
    # Super-sample (with replacement) from available row indices
    new_samples = sample(1:nrow(df), to_generate, replace = TRUE)
    # Create new dataframe using sampled row indices
    super_sampled = df[new_samples,]
    # Combine newly sampled data with original rows
    full_set = bind_rows(df, super_sampled)

    return (full_set)
  })

  # Combine into a single dataframe
  return (bind_rows(super_sampled))
}


if (!exists("balanced_train_data")) {
  source("data/data-sampling.R")
  balanced_train_data_filename = "balanced_train_data.RData"
  if (file.exists(balanced_train_data_filename)) {
    load(file = balanced_train_data_filename)
  }
  else {
    balanced_train_data = (balance_data(sentence_data$train, "president"))
    save(balanced_train_data, file = balanced_train_data_filename)
  }
}
