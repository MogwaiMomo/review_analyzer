library(rvest)

#### UDACITY ####

# catlog page to try to scrape 
url <- "https://www.udacity.com/courses/all"
html <- read_html(url)

body_nodes <- html %>%
  html_node("body") %>%
  html_children()

cards <- body_nodes %>%
  xml2::xml_find_all(".//li[contains(@class, 'catalog-cards__list__item')]")

catalog_df <- tibble::tibble(
  
  product_name = cards %>% 
    xml2::xml_find_first(".//h2") %>%
    rvest::html_text() %>%
    str_trim(side ="both"),
  
  school_name = cards %>% 
    xml2::xml_find_first(".//h3") %>%
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
    str_trim(side ="both"),
  
  url = cards %>%
    xml2::xml_find_first(".//a") %>%
    rvest::html_attr('href') %>%
    str_trim(side ="both")
)



# create full urls to look up & remove empty cols
catalog_df <- catalog_df %>%
  mutate(full_url = paste0("https://www.udacity.com", url))

catalog_df <- catalog_df %>%
  select(-c("prog_type","difficulty","skills","description","url"))

# get the missing metadata from old csv
original_catalog <- fread("output/udacity-catalog-2.csv")

# combine them
combined_df <- inner_join(original_catalog,catalog_df)

# differentiate by cost
catalog_df <- combined_df %>%  
  mutate(cost = ifelse(grepl("--nd", full_url), "paid", "free"))

# isolate paid
nds_only <- catalog_df %>%
  filter(cost == "paid")



# base output df
final_df <- data.frame()

for (i in nds_only$full_url) {
  
  print(i)
  
  html <- read_html(i)
  
  body_nodes <- html %>%
    html_node("body") %>%
    html_children()
  
  courses <- body_nodes %>%
    xml2::xml_find_all(".//li/div[contains(@class, 'part')]")
  
  course_df <- tibble::tibble(
    
    full_url = i,
    
    course_name = courses %>% 
      xml2::xml_find_first(".//h4") %>%
      rvest::html_text() %>%
      str_trim(side ="both"),
    
    project_FIRST_ONLY = courses %>%
      xml2::xml_find_first(".//button[contains(@class, 'part__project text-only')]/span") %>%
      rvest::html_text() %>%
      str_trim(side ="both")
  )
  
  final_df <- rbind(final_df, course_df)
  
}

print("Done!")

master_df <- inner_join(page_df, final_df)



# isolate free
free_only <- catalog_df %>%
  filter(cost == "free")


# base output df
free_final_df <- data.frame()

for (j in free_only$full_url) {
  
  print(j)
  
  html <- read_html(j)
  
  body_nodes <- html %>%
    html_node("body") %>%
    html_children()
  
  entry_nd <- body_nodes %>%
    xml2::xml_find_all(".//a[contains(@class, 'card__ndop--link')]")
  
  entry_nd_df <- tibble::tibble(
    
    free_course_url = j,
    
    entry_nd_url = entry_nd %>% 
      xml2::xml_find_all("//a[contains(@class, 'card__ndop--link')]") %>%
      rvest::html_attr('href') %>%
      str_trim(side ="both")
    
  )
  free_final_df <- rbind(free_final_df, entry_nd_df)
}

free_course_df <- free_final_df %>%
  mutate(full_entry_nd_url = paste0("https://www.udacity.com/course/", entry_nd_url)) %>%
  select(-entry_nd_url)

#rename url col for inner_join:
free_course_df <- free_course_df %>% 
  rename(full_url = free_course_url)

full_free_course_data <- inner_join(free_only, free_course_df)

# get the nd names & urls for join
nds_name_url <- nds_only %>%
  select(entry_nanodegree = product_name, 
         full_entry_nd_url = full_url)

#inner_join with free courses 

full_free_course_master <- inner_join(full_free_course_data,
                                      nds_name_url)

# print the datasets
full_free_course_master %>%
  distinct() -> free_courses_final_for_print

master_df %>%
  distinct() -> paid_courses_final_for_print

fwrite(free_courses_final_for_print, "output/udacity_free_courses_final.csv")

fwrite(paid_courses_final_for_print, "output/udacity_paid_courses_final.csv")

