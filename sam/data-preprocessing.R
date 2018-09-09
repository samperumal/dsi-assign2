require(tidyverse)
require(tidytext)

filePath = "sona-text-1994-2018/"

extractData = function(filePath) {
  files = list.files(filePath, full.names = TRUE)

  parseFilename = function(filename) {
    matches = str_match(filename, "[^/]*/?(\\d{4})_(post|pre|)(?:_elections_|)(.+)\\.txt")
    return (matches)
  }

  parseFile = function(filename) {
    con = file(filename, open = "r")
    lines = readLines(con, warn = FALSE)

    close(con)

    lines = lines[str_length(lines) > 0]

    matches = parseFilename(filename)

    result = data.frame(speech = unlist(lines), year = matches[2], election = matches[3], president = matches[4], stringsAsFactors = FALSE)

    return(result)
  }

  sentences = lapply(files, parseFile) %>% bind_rows() %>%
    unnest_tokens(sentence, speech, token = "sentences") %>%
    rowwise() %>%
    mutate(id = digest::digest(sentence)) %>%
    ungroup()

  presidents = sentences %>% select(president) %>% distinct()

  return (list(
    presidents = presidents,
    sentences = sentences,
    words = sentences %>% unnest_tokens(word, sentence, token = "words")
  ))
}

inputData = extractData(filePath)

#View(inputData$sentences)
#View(inputData$words)
