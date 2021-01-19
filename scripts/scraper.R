library(rvest)

# starting iterator
i <- 1
# base df
final_df <- data.frame()

# cycle through pages

while (i < 249) {

  # client page to try to scrape (trustpilot): 
  url <- "https://ca.trustpilot.com/review/www.bitdefender.com?page=%s"
  new_url <- sprintf(url, i)
  
  html <- read_html(new_url)
  
  body_nodes <- html %>%
    html_node("body") %>%
    html_children()
  
  reviews <- body_nodes %>%
    xml2::xml_find_all(".//div[contains(@class, 'review-card  ')]")
  
  page_df <- tibble::tibble(
    name = reviews %>% 
      xml2::xml_find_first(".//div[contains(@class, 'consumer-information__name')]") %>%
      rvest::html_text() %>%
      str_trim(side ="both"),
    
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
  
  final_df <- rbind(final_df, page_df)
  i <- i+1
  
}






fwrite(review_data, file="output/reviews.csv")
review_data <- fread("output/reviews.csv")

# Next steps: 

# DONE - clean and format what should be number vars



# cycle through a set number of pages

# break if max pages exceeded

# turn into function



# Info to scrape:

# name:           consumer-information__name
# no. of reviews: consumer-information__review-count > span
# rating:         rating.star-rating--medium > img alt (?)
# review text:    review-content__body > p
# CAN'T GET THIS ONE: timestamp:      review-content-header__dates > div > span > time

