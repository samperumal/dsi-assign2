customFun  = function(DF) {
  write.table(DF, unique(DF$filename))
  return(DF)
}

output = inputData$sentences %>%
  mutate(filename = paste0(year, election, president, ".txt"), sentence = paste0("XXX", sentence)) %>%
  select(filename, sentence) %>%
  group_by(filename) %>%
  do(customFun(.))

head(output)
