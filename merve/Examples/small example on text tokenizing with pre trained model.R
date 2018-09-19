library(keras)
docs = c('Well done!',
        'Good work',
        'Great effort',
        'nice work',
        'Excellent!',
        'Weak',
        'Poor effort!',
        'not good',
        '"poor" "work"',
        'Could have done better.')
# define class labels
labels = c(1,1,1,1,1,0,0,0,0,0)

tokenizer = keras::text_tokenizer(num_words = 19, filters = "!\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n")
tokenizer$fit_on_texts(docs)
docs = tokenizer$texts_to_sequences(docs)

x_data = pad_sequences(docs, 4, padding = "post", truncating = "post") #4 is max number of words

tokenizer$sequences_to_texts(x_data)

tokenizer <- load_text_tokenizer("tokenizer")
tokenizer$fit_on_texts(docs)
docs = tokenizer$texts_to_sequences(docs)
x_data = pad_sequences(docs, 4, padding = "post", truncating = "post") #4 is max number of words

