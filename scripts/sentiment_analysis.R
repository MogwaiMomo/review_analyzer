library("sentimentr")

# SA for individual words
get_sentiments_words <- function(text_df, omit_words, lexicon, emotion){
  # create tidy tokens for analysis
  tidy_tweets <- create_tidy_tokens(text_df, "text", omit_words)
  
  if (lexicon == "afinn") {
    
    if (emotion == "positive") {
      sent_dataset <- get_sentiments(lexicon) %>%
        filter(value >= 3) 
      
      sentiment_words <- tidy_tweets %>%
        inner_join(sent_dataset) %>%
        count(word, sort = T)
      return(sentiment_words)
      
    } else if (emotion == "negative") {
      sent_dataset <- get_sentiments(lexicon) %>%
        filter(value <= -3) 
      
      sentiment_words <- tidy_tweets %>%
        inner_join(sent_dataset) %>%
        count(word, sort = T)
      
      return(sentiment_words)
    }
  } else {
    sent_dataset <- get_sentiments(lexicon) %>%
      filter(sentiment == emotion)
    
    sentiment_words <- tidy_tweets %>%
      inner_join(sent_dataset) %>%
      count(word, sort = T)
    
    return(sentiment_words)
  }
}  

# function for clustering by sentiment using sentimentR (sentence-level analysis)

# https://cran.r-project.org/web/packages/sentimentr/readme/README.html#preferred-workflow

# SA for full documents (tweets in this case)

document_level_sa <- function(df) {
  
  # create an element_id col based on row name
  rowid_to_column(df) %>%
    # rename rowid col to element_id
    rename(element_id = rowid) -> df
    
  # calculate sentiment for each tweet 
  df %>% 
    get_sentences() %>%
    sentiment_by(by = 'element_id') -> sent_by_df 
  
  # outer-join tweets with sent by tweet
  df %>%
    full_join(sent_by_df, by = 'element_id') -> df_with_sent
  
  return(df_with_sent)
}







  
  


  



