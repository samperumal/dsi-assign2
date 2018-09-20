require(tidyverse)
require(tidytext)

filePath = "sona-text-1994-2018/"

extractData = function(filePath) {
  files = list.files(filePath, full.names = TRUE)

  parseFilename = function(filename) {
    matches = str_match(filename, "[^/]*/?(\\d{4})_(post|pre|)(?:_elections_|) *(.+)\\.txt")
    return (matches)
  }

  parseFile = function(filename, apply_filter = TRUE) {
    con = file(filename, open = "r", encoding = "UTF-8")
    lines = readLines(con, warn = TRUE)

    close(con)

    # Omit empty lines
    lines = (lines[str_length(lines) > 0])

    if (apply_filter) {
      # Remove uneccessary non-word characters.
      lines = str_replace_all(lines, "[\"“”%’‘\\[\\]\\(\\)–+¬>-]", "")
      # Replace slashes with spaces
      lines = str_replace_all(lines, "[/]", " ")

      # Replace bullet points, force leading character to uppercase for sentence breakdown
      lines = str_replace_all(lines, "^ *\\* +(.+)$", "\\1\\.")

      # Remove currency values and numbers
      lines = str_replace_all(lines, "[r$]? ?\\d+([,\\.]\\d+)*", "")

      # Change ellipsis, :, to a full-stop
      lines = str_replace_all(lines, "(\\.{2,}|[:;…¦])", ". ")

      # Remove full stops separated only by whitespace
      lines = str_replace_all(lines, "(\\.( +\\.)+)", "\\.")

      # Collapse whitespace
      lines = str_replace_all(lines, " {2,}", " ")

      # Ensure the first character after every full stop is made uppercase, so sentence tokenisation works correctly
      lines = str_replace_all(lines, "((\\. +)|(^ *))([a-z])", toupper)
    }

    # Extract parts of filename
    matches = parseFilename(filename)

    result = data.frame(speech = lines,
                        president = matches[4],
                        year = as.integer(matches[2]),
                        election = matches[3],
                        stringsAsFactors = FALSE)

    return(result)
  }

  filtered_lines = lapply(files, parseFile) %>% bind_rows()
  unfiltered_lines = lapply(files, parseFile, apply_filter = FALSE) %>% bind_rows()

  sentences = filtered_lines %>%
    unnest_tokens(sentence, speech, token = "sentences", collapse = TRUE) %>%
    rowwise() %>%
    mutate(id = digest::digest(sentence)) %>%
    ungroup() %>%
    # remove dated prefaces
    filter(!grepl("^(february|may|deputy speaker|mr lovemore moyo)", sentence))

  # Find duplicate sentence ids
  duplicate_ids = sentences %>%
    group_by(id) %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    select(id) %>%
    unique()

  # Remove duplicate sentences
  sentences = sentences %>% anti_join(duplicate_ids, by = "id")

  presidents = sentences %>% select(president) %>% distinct()

  words = sentences %>% unnest_tokens(word, sentence, token = "words")

  unfiltered_sentences = unfiltered_lines %>%
    unnest_tokens(sentence, speech, token = "sentences", collapse = TRUE) %>%
    rowwise() %>%
    mutate(id = digest::digest(sentence)) %>%
    ungroup()

  unfiltered_words = unfiltered_sentences %>% unnest_tokens(word, sentence, token = "words")

  return (list(
    presidents = presidents,
    sentences = sentences,
    words = words,
    unfiltered_sentences = unfiltered_sentences,
    unfiltered_words = unfiltered_words,
    filtered_lines = filtered_lines
    #riginal_sentences = original_sentences
  ))
}

if (!exists("input_data")) {
  input_filename = "input_data.RData"
  if (file.exists(input_filename))
  {
    load(file = input_filename)
  }
  else {
    input_data = extractData(filePath)
    save(input_data, file = input_filename)
  }
}

write_debug_data = function () {
  input_data$sentences %>%
    mutate(slen = str_length(sentence)) %>%
    filter(slen > 400) %>%
    arrange(desc(slen)) %>%
    #filter(!grepl("[^A-Za-z.,!?' -]", sentence)) %>%
    #select(id, sentence) %>%
    write.table(file = "long_sentences.txt")

  write.table(input_data$filtered_lines, file = "filtered_lines.txt")
  write.table(input_data$sentences, file = "sentences.txt")
  write.table(input_data$sentences %>% filter(year == "2008"), file = "sentences_check.txt")
}
