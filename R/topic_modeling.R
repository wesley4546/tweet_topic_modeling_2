source(here::here("R", "cleaning_functions.R"))
library(stm)

# Data Cleaning/Wrangling -------------------------------------------------

raw_data <- get_raw_data()

cleaned_data <-
  raw_data %>%
  select(created_at, complete_texts) %>%
  mutate(complete_texts = remove_urls(complete_texts)) %>%
  mutate(complete_texts = remove_non_alphabet_characters(complete_texts))

# Pattern for removing anything after 2 digits
pattern <- "([[:digit:]])([[:digit:]]).*"
replacement <- "\\1\\2"

cleaned_data <-
  cleaned_data %>%
  mutate(created_at = gsub(x = created_at, pattern = pattern, replacement = replacement))

# Removes day of the week
pattern <- "(Sat|Sun|Mon|Tue|Wed|Thu|Fri)"
replacement <- ""

cleaned_data <-
  cleaned_data %>%
  mutate(created_at = gsub(x = created_at, pattern = pattern, replacement = replacement))

# adds year to the column and changes format
cleaned_data <-
  cleaned_data %>%
  mutate(created_at = (paste("2020", created_at, sep = ""))) %>%
  mutate(created_at = gsub(x = created_at, pattern = "Mar", replacement = "03")) %>%
  mutate(created_at = gsub(x = created_at, pattern = "Apr", replacement = "04")) %>%
  mutate(created_at = ymd(`created_at`))

# Gets stopwords
custom_stopwords <-
  get_hagen_stopwords() %>%
  bind_rows(get_wesley_stopwords())

# Topic Modeling ----------------------------------------------------------

# Processes the text
processed <-
  textProcessor(
    documents = cleaned_data$complete_texts,
    stem = FALSE,
    removestopwords = FALSE,
    customstopwords = custom_stopwords$word,
    metadata = cleaned_data
  )

# Prepares documents for topic modeling
prepared_documents <-
  prepDocuments(processed$documents, processed$vocab, processed$meta)

# Assigns the different outputs to more convenient variables
docs <- prepared_documents$documents
vocab <- prepared_documents$vocab
meta <- prepared_documents$meta

tictoc::tic()

range_of_topics <- seq(5, 40, 5)

findingk <-
  searchK(
    documents = prepared_documents$documents,
    vocab = prepared_documents$vocab,
    K = range_of_topics,
    data = meta
  )

tictoc::toc()

model_metrics <- (plot(findingk))

ggsave(
  filename = here::here("output","graphs", paste0(model_metrics,".png")),
  plot = model_metrics
)

save(list = ls(), file = here::here("output","findingk.rda"))
