library(rvest)
# Work through this so you can scrape any review site

# https://towardsdatascience.com/tidy-web-scraping-in-r-tutorial-and-resources-ac9f72b4fe47

# Client page to try to scrape (trustpilot): 

url <- "https://ca.trustpilot.com/review/www.bitdefender.com"
html <- read_html(url)

body_nodes <- html %>%
  html_node("body") %>%
  html_children()

reviews <- body_nodes %>%
  xml2::xml_find_all(".//div[contains(@class, 'review-card  ')]")


consumer-information__review-count

review_data <- tibble::tibble(
  name = reviews %>% 
    xml2::xml_find_first(".//div[contains(@class, 'consumer-information__name')]") %>%
    rvest::html_text() %>%
    str_trim(side ="both"),
  num_reviews = reviews %>%
    xml2::xml_find_first(".//div[contains(@class, 'consumer-information__review-count')]/span") %>%
    rvest::html_text() %>%
    str_trim(side ="both"),
  rating = reviews %>%
    xml2::xml_find_first(".//div[contains(@class, 'star-rating')]/img") %>%
    rvest::html_attr('alt') %>%
    str_trim(side ="both"),
  text = reviews %>%
    xml2::xml_find_first(".//p[contains(@class, 'review-content__text')]") %>%
    rvest::html_text() %>%
    str_trim(side ="both")
)
  



# Info to scrape:

# name:           consumer-information__name
# no. of reviews: consumer-information__review-count > span
# rating:         rating.star-rating--medium > img alt (?)
# review text:    review-content__body > p
# CAN'T GET THIS ONE: timestamp:      review-content-header__dates > div > span > time

