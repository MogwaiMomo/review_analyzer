options(stringsAsFactors = F)

# libraries for inputting data
library(httr)
library(jsonlite)
library(rlist)

# libraries for tidying
library(magrittr)
library(data.table)
library(tidyverse)
library(tidytext)
library(janitor)
library(stringr)

# libraries for viz
library(ggplot2)
library(wordcloud)
library(gridExtra)

# libraries for NLP
library(topicmodels)
library(tm)
library(hunspell)
library(textcat)


source("scripts/explore_trends.R")
source("scripts/create_wordcloud.R")
source("scripts/tidy_tokens.R")
source("scripts/sentiment_analysis.R")


# Uncomment, edit & pull from reviewapi if need be
# url <- "https://app.reviewapi.io/api/v1/reviews?apikey=48872790-503d-11eb-a971-65901c93bc91&url=https%3A%2F%2Fwww.capterra.com%2Fp%2F140650%2FRecruitee%2Freviews&amount=25"
# r <- GET(url)
# r.list <- fromJSON(httr::content(r, as = "text"))
# data <- r.list$reviews


# Uncomment, edit file & scrape review sites
# source("scripts/scraper.R")



# Load the data
f <- "output/bitdefender_reviews.csv"
data <- fread(f)

# add element_id based on rowid
data <- create_id_var(data)

# explore the included data
str(data)

# Uncomment, edit & change format, if needed

# data$____ <-  as.Date(data$____)
# data$____ <-  as.factor(review_data$____)
# data$____ <-  as.numeric(review_data$____)
# data$____ <-  as.character(review_data$____)

# Uncomment, edit & flatten the data frame, if need be

# df <- data$____
# data %>%
#   select(-____) -> df2
# data <- inner_join(df, df2, by = "element_id")
# data <- as_tibble(data)
# data %>%
#   select(-c(text, title)) -> data
  

quants <- isolate_quants(data)
quals <- isolate_quals(data)
texts <- isolate_texts(data) 


tidy_texts <- texts %>%
  mutate(language = textcat(text)) %>% # identify languages
  filter(language == "english") %>% # filter only eng documents 
  select(-language)

filename <- "output/corr_plots.png"
plot_quants(filename, quants, "element_id")

# skip boxplot analysis - no real categories to use

# sentiment analysis with sentimentR
sent_df <- document_level_sa(texts)

  

