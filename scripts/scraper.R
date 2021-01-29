library(rvest)


# client page to try to scrape (trustpilot): 
url <- "https://www.g2.com/products/g2/reviews#reviews"

html <- read_html(url)

body_nodes <- html %>%
  html_node("body") %>%
  html_children()

reviews <- body_nodes %>%
  xml2::xml_find_all(".//div[contains(@class, 'paper paper--white paper--box mb-2 position-relative border-bottom')]")



page_df <- tibble::tibble(
  name = reviews %>% 
    xml2::xml_find_first(".//h2[@class='card__title__nd-name']") %>%
    rvest::html_text() %>%
    str_trim(side ="both")
)


  num_reviews = reviews %>%
    xml2::xml_find_first(".//div[contains(@class, 'consumer-information__review-count')]/span") %>%
    rvest::html_text() %>%
    str_replace_all("\\D+", "") %>%
    str_trim(side ="both") %>%
    as.numeric(),
  
  rating = reviews %>%
    xml2::xml_find_first(".//div[contains(@class, 'star-rating')]/img") %>%
    rvest::html_attr('alt') %>%
    str_replace_all("\\D+", "") %>%
    str_trim(side ="both") %>%
    as.numeric(),
  
  text = reviews %>%
    xml2::xml_find_first(".//p[contains(@class, 'review-content__text')]") %>%
    rvest::html_text() %>%
    str_trim(side ="both")
)
  


# fwrite(final_df, file="output/bitdefender_reviews.csv")
# review_data <- fread("output/bitdefender_review.csv")



