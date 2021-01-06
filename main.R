options(stringsAsFactors = F)

# libraries for inputting data
library(httr)
library(jsonlite)

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


r <- GET("https://app.reviewapi.io/api/v1/reviews?apikey=48872790-503d-11eb-a971-65901c93bc91&url=https%3A%2F%2Fwww.amazon.com%2FTUSHY-Classic-Bidet-Toilet-Attachment%2Fdp%2FB07B8Y327H&amount=30")

