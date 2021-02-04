#### UDACITY ####

# client page to try to scrape 
url <- "https://www.udacity.com/courses/all"
html <- read_html(url)

body_nodes <- html %>%
  html_node("body") %>%
  html_children()

cards <- body_nodes %>%
  xml2::xml_find_all(".//li[contains(@class, 'catalog-cards__list__item')]")

page_df <- tibble::tibble(
  
  product_name = cards %>% 
    xml_find_first(".//h2[contains(@class, 'card__title__nd-name')]") %>%
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


fwrite(page_df, file="output/udacity-catalog-2.csv")