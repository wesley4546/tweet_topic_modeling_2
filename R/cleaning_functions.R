library(tidyverse)
library(lubridate)

get_raw_data <- function(){
  
  out <- list.files(here::here("data"),
                    pattern = "*.csv",
                    full.names = TRUE) %>% 
    map_df(~read_csv(.))
  
  return(out)
  
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
