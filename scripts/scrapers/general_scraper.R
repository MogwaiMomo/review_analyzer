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
    
    new_url <- sprintf(url, i)
    html <- read_html(new_url)
    
    print(new_url)
    
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
base_url <- "https://ca.trustpilot.com/review/www.plushcare.com?page=%s"
company <- "plushcare"
total_reviews <- 751

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





