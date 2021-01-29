library(rvest)

# To get around 403 error

# Solution:
#   function used in httr add_headers package () Set User-Agent (browser identification flag) HTTP request;

# function html_session package rvest reuse in () crawling information, html_nodes () function is used to find the label, html_text () for extracting the text label,%> a piping% operators


# client page to try to scrape: 
url <- "https://www.g2.com/products/g2/reviews#reviews"

session <- url %>% 
  html_session(
    add_headers(
      `User-Agent`="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.96 Safari/537.36"
      )
    ) %>% 
  html_nodes("body")%>%
  html_text()






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


#   num_reviews = reviews %>%
#     xml2::xml_find_first(".//div[contains(@class, 'consumer-information__review-count')]/span") %>%
#     rvest::html_text() %>%
#     str_replace_all("\\D+", "") %>%
#     str_trim(side ="both") %>%
#     as.numeric(),
#   
#   rating = reviews %>%
#     xml2::xml_find_first(".//div[contains(@class, 'star-rating')]/img") %>%
#     rvest::html_attr('alt') %>%
#     str_replace_all("\\D+", "") %>%
#     str_trim(side ="both") %>%
#     as.numeric(),
#   
#   text = reviews %>%
#     xml2::xml_find_first(".//p[contains(@class, 'review-content__text')]") %>%
#     rvest::html_text() %>%
#     str_trim(side ="both")
# )
#   


# fwrite(page_df, file="output/reviews.csv")
# review_data <- fread("output/reviews.csv")



