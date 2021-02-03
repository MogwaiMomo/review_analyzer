library(rvest)

# Create scraping function
scrape_pages <- function(total_reviews, 
                         revs_per_page, 
                         url, 
                         company,
                         review_xpath,
                         name_xpath,
                         num_reviews_xpath,
                         rating_xpath,
                         text_xpath
                         ) {
  # base output df
  final_df <- data.frame()
  
  total_reviews <- as.numeric(total_reviews)
  revs_per_page <- as.numeric(revs_per_page)
 
  # start and end urls
  start_page <- 1
  end_page <- ceiling(total_reviews/revs_per_page)
  
  # scrape target items from every url
  for(i in seq(from=start_page, to=end_page, by=1)){
    
    print(i)
    
    new_url <- sprintf(url, i)
    html <- read_html(new_url)
    
    body_nodes <- html %>%
      html_node("body") %>%
      html_children()
    
    reviews <- body_nodes %>%
      xml2::xml_find_all(review_xpath)
    
    page_df <- tibble::tibble(
      name = reviews %>% 
        xml2::xml_find_first(name_xpath) %>%
        rvest::html_text() %>%
        str_trim(side ="both"),
      
      num_reviews = reviews %>%
        xml2::xml_find_first(num_reviews_xpath) %>%
        rvest::html_text() %>%
        str_replace_all("\\D+", "") %>%
        str_trim(side ="both") %>%
        as.numeric(),
      
      rating = reviews %>%
        xml2::xml_find_first(rating_xpath) %>%
        rvest::html_attr('alt') %>%
        str_replace_all("\\D+", "") %>%
        str_trim(side ="both") %>%
        as.numeric(),
      
      text = reviews %>%
        xml2::xml_find_first(text_xpath) %>%
        rvest::html_text() %>%
        str_trim(side ="both")
    )
    
    final_df <- rbind(final_df, page_df)
  }
  
  output_file <- paste0("output/", company, "_reviews_automated.csv")
  fwrite(final_df, file=output_file)
  rm(final_df)
}

#### RUN IT ####
base_url <- "https://ca.trustpilot.com/review/www.sky.com"
company <- "sky"
total_reviews <- 1000

revs_per_page <- 20
review_xpath <- ".//div[contains(@class, 'review-card  ')]"
name_xpath <- ".//div[contains(@class, 'consumer-information__name')]"
num_reviews_xpath <- ".//div[contains(@class, 'consumer-information__review-count')]/span"
rating_xpath <- ".//div[contains(@class, 'star-rating')]/img"
text_xpath <- ".//p[contains(@class, 'review-content__text')]"

scrape_pages(total_reviews,
             revs_per_page,
             base_url, 
             company,
             review_xpath,
             name_xpath,
             num_reviews_xpath,
             rating_xpath,
             text_xpath
             )





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

