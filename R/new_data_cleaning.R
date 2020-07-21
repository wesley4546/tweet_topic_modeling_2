library(dplyr)
library(stringr)

# Gets raw data
get_raw_data <- function() {
  # Reads the two data in
  first_set <- read.csv(here::here("data", "first_2weeks.csv"), encoding = "UTF-8-B0M")
  second_set <- read.csv(here::here("data", "last_2weeks.csv"), encoding = "UTF-8-B0M")
  
  # Binds them
  raw_data <-
    first_set %>%
    bind_rows(second_set)
  
  colnames(raw_data)[1] <- "tweet_created_at" 

  return(raw_data)
}

# Finds unique tweets
get_unique_tweets <- function(raw_data) {

  # Removes any potential duplicates

  # Gets tweets
  tweets_only <-
    raw_data %>%
    filter(is.na(retweet_text)) %>%
    select(tweet_created_at, tweet_text)

  # Gets retweets
  retweets_only <-
    raw_data %>%
    filter(!is.na(retweet_text)) %>%
    select(tweet_created_at, retweet_text) %>%
    rename(tweet_text = retweet_text)

  # Gets quoted
  quoted_only <-
    raw_data %>%
    filter(!is.na(quoted_text)) %>%
    select(tweet_created_at, quoted_text) %>%
    rename(tweet_text = quoted_text)

  # Combines them
  all_tweets <-
    tweets_only %>%
    bind_rows(retweets_only, quoted_only)

  # Gets the unique occurrences
  unique_tweets <-
    all_tweets %>%
    distinct(tweet_text, .keep_all = TRUE)

  # Gets statistics of tweets
  num_total_tweets <- nrow(all_tweets)
  num_unique_tweets <- nrow(unique_tweets)
  percentage_tweets <- (num_unique_tweets / num_total_tweets) * 100

  # Output message
  message(paste(
    "Found", num_unique_tweets,
    "out of", num_total_tweets,
    "unique tweets,",
    round(percentage_tweets, 2), "% unique tweets."
  ))

  return(unique_tweets)
}

# Helper functions --------------------------------------------------------

remove_urls <- function(tweet_text) {

  # Removes URLs
  url_regex_expression <- "(http|https)://([^\\s]+)"

  tweet_text <- str_remove_all(
    string = tweet_text,
    pattern = url_regex_expression
  )

  return(tweet_text)
}

remove_non_alphabet_characters <- function(tweet_text) {

  # Regex for characters
  alphabet_regex_expression <- "[^\x20-\x7E]"

  # Removes characters
  tweet_text <-
    str_remove_all(
      string = tweet_text,
      pattern = alphabet_regex_expression
    )

  return(tweet_text)
}

get_hagen_stopwords <- function() {
  # Gets the stopwords from Dr. Hagen's txt
  hagen_stopwords <-
    read.table(
      file = here::here("data", "hagen_stopwords.txt"),
      header = FALSE,
      sep = "\n",
      stringsAsFactors = FALSE
    ) %>%
    as_tibble() %>%
    rename(word = V1)

  # Removes any whitespace
  hagen_stopwords <-
    hagen_stopwords %>%
    mutate(word = trimws(word))

  return(hagen_stopwords)
}

get_keywords <- function() {
  # Gets the keywords used in collecting
  keywords <-
    read.table(
      file = here::here("data", "dres_keywords.txt"),
      header = FALSE,
      sep = "\n",
      stringsAsFactors = FALSE
    ) %>%
    as_tibble() %>%
    rename(word = V1)

  # Removes any whitespace
  keywords <-
    keywords %>%
    mutate(word = trimws(word))

  return(keywords)
}

get_wesley_stopwords <- function() {
  # Gets the keywords used in collecting
  stopwords <-
    read.table(
      file = here::here("data", "wesley_stopwords.txt"),
      header = FALSE,
      sep = "\n",
      stringsAsFactors = FALSE
    ) %>%
    as_tibble() %>%
    rename(word = V1)
  
  # Removes any whitespace
  stopwords <-
    stopwords %>%
    mutate(word = trimws(word))
  
  return(stopwords)
}
