library(rvest)


#### PLUSHCARE ####

# starting iterator
i <- 1
# base df
final_df <- data.frame()

# cycle through pages

while (i < 39) {

  # client page to try to scrape (trustpilot): 
  url <- "https://www.trustpilot.com/review/www.plushcare.com?page=%s"
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


fwrite(final_df, file="output/plushcare_reviews.csv")
rm(final_df)


#### UDACITY ####

library(rvest)

# client page to try to scrape 
url <- "https://www.udacity.com/courses/all"
html <- read_html(url)

body_nodes <- html %>%
  html_node("body") %>%
  html_children()

cards <- body_nodes %>%
  xml2::xml_find_all(".//li[contains(@class, 'catalog-cards__list__item')]")

page_df <- tibble::tibble(
  
  product_name <- cards %>% 
    xml_find_first(".//h2") %>%
    rvest::html_text() %>%
    str_trim(side ="both"),
  
  school_name = cards %>% 
    xml2::xml_find_first(".//h3[contains(@class, 'card__title__school')]") %>%
    rvest::html_text() %>%
    str_trim(side ="both"),
  
  prog_type = cards %>% 
    xml2::xml_find_first(".//div[contains(@class, 'card__image-overlay')]") %>%
    rvest::html_attr('data-catalogtype') %>%
    str_trim(side ="both"),
  
  difficulty = cards %>% 
    xml2::xml_find_first(".//div[contains(@class, 'difficulty')]/div") %>%
    rvest::html_attr('data-level') %>%
    str_trim(side ="both"),
  
  skills = cards %>% 
    xml2::xml_find_first(".//p[contains(@class, 'text-content__text')]") %>%
    rvest::html_text() %>%
    str_trim(side ="both"),
  
  description = cards %>% 
    xml2::xml_find_first(".//div[contains(@class, 'catalog-component__details')]") %>%
    rvest::html_text() %>%
    str_trim(side ="both")
  
)


fwrite(page_df, file="output/udacity-catalog.csv")

