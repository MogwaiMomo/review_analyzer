library(rvest)
# Work through this so you can scrape any review site

# https://towardsdatascience.com/tidy-web-scraping-in-r-tutorial-and-resources-ac9f72b4fe47

# Client page to try to scrape (trustpilot): 

url <- "https://ca.trustpilot.com/review/www.bitdefender.com"
html <- read_html(url)

body_nodes <- html %>%
  html_node("body") %>%
  html_children()

name <- body_nodes %>%
  xml2::xml_find_all("//div[@class='consumer-information__name']") %>% 
  rvest::html_text() %>%
  str_trim(side ="both")

text <- body_nodes %>%
  xml2::xml_find_all("//p[@class='review-content__text']") %>%
  rvest::html_text() %>%
  str_trim(side ="both")




# Info to scrape:

# DONE name:           consumer-information__name
# no. of reviews: consumer-information__review-count > span
# rating:         rating.star-rating--medium > img alt (?)
# DONE review text:    review-content__body > p
# timestamp:      review-content-header__dates > div > span > time