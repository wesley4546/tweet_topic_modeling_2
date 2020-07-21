library(stm)
library(lubridate)

source(here::here("R", "new_data_cleaning.R"))


# Reading Data ------------------------------------------------------------

data <- get_raw_data()

cleaned_data <-
  get_unique_tweets(data) %>%
  mutate(tweet_text = remove_urls(tweet_text)) %>%
  mutate(tweet_text = remove_non_alphabet_characters(tweet_text))


# Ugly cleaning ahead -----------------------------------------------------

# Pattern for removing anything after 2 digits
pattern <- "([[:digit:]])([[:digit:]]).*"
replacement <- "\\1\\2"

cleaned_data <-
  cleaned_data %>%
  mutate(tweet_created_at = gsub(
    x = tweet_created_at,
    pattern = pattern,
    replacement = replacement
  ))

# Removes day of the week
pattern <- "(Sat|Sun|Mon|Tue|Wed|Thu|Fri)"
replacement <- ""

cleaned_data <-
  cleaned_data %>%
  mutate(tweet_created_at = gsub(
    x = tweet_created_at,
    pattern = pattern,
    replacement = replacement
  ))

# adds year to the column
cleaned_data <-
  cleaned_data %>%
  mutate(tweet_created_at = (paste("2020", tweet_created_at, sep = ""))) %>%
  mutate(tweet_created_at = gsub(
    x = tweet_created_at,
    pattern = "Mar",
    replacement = "03"
  )) %>%
  mutate(tweet_created_at = gsub(
    x = tweet_created_at,
    pattern = "Apr",
    replacement = "04"
  )) %>%
  mutate(tweet_created_at = ymd(`tweet_created_at`))






# If I want only unique tweets
# pattern <- "([A-z])([[:punct:]])([A-z])"
#
# replacement <- "\\1\\2 \\3"
#
# cleaned_data$tweet_text <- gsub(pattern,replacement,cleaned_data$tweet_text)
#
# cleaned_data$tweet_text[2664]
#
# cleaned_data$tweet_text[2754]
#
# cleaned_data <-
#   cleaned_data %>%
#   distinct(tweet_text, .keep_all = TRUE)

custom_stopwords <-
  get_hagen_stopwords() %>%
  bind_rows(get_wesley_stopwords())


# Ingest ------------------------------------------------------------------

processed <- textProcessor(
  documents = cleaned_data$tweet_text,
  stem = FALSE,
  removestopwords = FALSE,
  customstopwords = custom_stopwords$word,
  metadata = cleaned_data
)

# visualize lower.thresh's impact on documents
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 1))

# lower.thresh is the minimum number of documents a word needs to appear
# in order for the word to be kept within the vocabulary.
pre <- prepDocuments(processed$documents, processed$vocab, processed$meta,
  lower.thresh = 2
)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Evaluate ----------------------------------------------------------------

tictoc::tic()
findingk <- searchK(
  documents = out$documents,
  vocab = out$vocab,
  K = seq(5, 40, 5),
  data = meta
)
tictoc::toc()

plot(findingk)
# Estimate ----------------------------------------------------------------

tictoc::tic()
topic_model <- stm(
  documents = out$documents, vocab = out$vocab,
  K = 25,
  data = out$meta,
  init.type = "Spectral",
  verbose = TRUE
)
tictoc::toc()

# Get the indices of the removed documents of prepped
index_docs_removed <- append(processed[["docs.removed"]], out[["docs.removed"]])

# apply them
saved_documents <- cleaned_data[-index_docs_removed, ]


# Function that gets the indices of a topic in a model
get_topic_index <- function(model, texts, n, topic, thresh) {
  
  # Uses the findThoughts function
  thoughts_object <- findThoughts(model,
    texts = texts,
    n = n,
    topics = topic,
    thresh = thresh
  )


  return(thoughts_object$index)
}

topic_1_indices <- 
  get_topic_index(
    model = topic_model,
    texts = documents,
    n = number_of_documents,
    topic = 1,
    thresh = threshhold
)

k <- seq(1,25,1)

# Generates indices for topics
for(i in k){
  
  assign(paste0("topic_",i,"_indices"),
         get_topic_index(
           model = topic_model,
           texts = documents,
           n = number_of_documents,
           topic = i,
           thresh = threshhold
         )
  )
}

# Gets variables
total_topics <- 25 
documents <- saved_documents$tweet_text
number_of_documents <- length(saved_documents$tweet_text)
threshhold <- 0.51

for(topic_num in total_topics){
  
  object_name <- paste0("topic_",topic_num,"_index")
  
  assign(object_name,
         get_topic_index(
           topic_model,
           documents,
           number_of_documents,
           topic_num,
           threshhold
         )
  )
}
