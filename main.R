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

r <- GET("https://app.reviewapi.io/api/v1/reviews?apikey=48872790-503d-11eb-a971-65901c93bc91&url=https%3A%2F%2Fwww.capterra.com%2Fp%2F140650%2FRecruitee%2Freviews&amount=25")

# success! the scraper works (at least for the example URL ...)

# Next goal - convert response data to a tibble:
# https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/

r.list <- fromJSON(httr::content(r, as = "text"))
reviews_data <- r.list$reviews


platform_specific <- reviews_data$platform_specific

source("scripts/explore_trends.R")
source("scripts/create_wordcloud.R")
source("scripts/tidy_tokens.R")
source("scripts/sentiment_analysis.R")

# Step 1: explore the included data
str(reviews_data)

# change format 
reviews_data$timestamp <- as.Date(reviews_data$timestamp)
reviews_data$platform_specific$user_job_title <- as.factor(reviews_data$platform_specific$user_job_title)
reviews_data$platform_specific$user_company_name <- as.factor(reviews_data$platform_specific$user_company_name)
reviews_data$platform_specific$user_industry <- as.factor(reviews_data$platform_specific$user_industry)

# add element_id based on rowid
reviews_data <- create_id_var(reviews_data)
reviews_data$platform_specific <- create_id_var(reviews_data$platform_specific)

# flatten the data frame
df <- reviews_data$platform_specific
reviews_data %>%
  select(-platform_specific) -> df2
data <- inner_join(df, df2, by = "element_id")
data <- as_tibble(data)
data %>%
  select(-c(text, title)) -> data
  

quants <- isolate_quants(data)
quals <- isolate_quals(data)
texts <- isolate_texts(data)

filename <- "output/corr_plots.png"
plot_quants(filename, quants)

# skip boxplots - no real categories to use


# 