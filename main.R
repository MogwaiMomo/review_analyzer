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
