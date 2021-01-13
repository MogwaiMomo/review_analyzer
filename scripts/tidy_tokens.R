create_tidy_tokens <- function(df, text, omit_words) {
  tidy_tweets <- df %>%
    mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
           text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
           text = str_remove_all(text, "[^\x01-\x7F]")) %>%
    unnest_tokens(word, text, token = "tweets") %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]"),
           !str_detect(word, "^#"),
           !str_detect(word, "@\\S+")) %>%
    filter(!word %in% omit_words)
  return(tidy_tweets)
}