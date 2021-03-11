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



# Uncomment, edit file & scrape review sites
source("scripts/scraper.R")

# Load the data
f <- "data/udacity_skills.csv"
data <- fread(f)

skill_list <- data %>%
  select(skills)

skills_raw <- as_tibble(t(str_split(skill_list, ", ", simplify=TRUE))) %>%
  rename(skill = "V1")

skills_final <- skills_raw %>%
  mutate(skills = str_replace_all(skill, coll("c("), ""))

s <- str_replace_all(skills_final$skills, coll(")"), "")

s <- str_replace_all(s, coll('"'), "")
s <- str_replace_all(s, coll('\n'), "") 
s <- str_replace_all(s, coll('\\n'), " ")
s <- str_trim(s)

s <- str_to_lower(s)
final <- as_tibble(unique(s)) %>%
  arrange(value)

fwrite(final, "~/Desktop/skill_list.csv")


# add element_id based on rowid
data <- create_id_var(data)

# explore the included data
str(data)

# Uncomment, edit & change format, if needed

# data$____ <-  as.Date(data$____)
data$rating <-  as.factor(data$rating)
# data$____ <-  as.numeric(data$____)
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

texts %>%
  mutate(language = textcat(text)) -> check_lang_df

# if need be, check for other languages
texts <- check_lang_df %>% 
  filter(language == "english") %>% # if need be, filter only eng documents 
  select(-language)

# skip boxplot analysis - no real categories to use

# keep star rating before doing sent analysis
texts <- inner_join(quals, texts)

# sentiment analysis with sentimentR
sent_df <- document_level_sa(texts) %>%
  filter(word_count != 0) # remove low-word count reviews (uninformative, likely high % of fakes)

# remove duplicate entries
sent_df <- subset(sent_df, !duplicated(
  subset(sent_df, select = "text")))

# Interesting question: how does topic diversity & substance change with word count? 

# First: how is the word count distributed? 

# before checking analysis, remove low word-count reviews

summary(sent_df) 

# Median is 43 words. Note that the variation in word count of the first ,second and 3rd quantiles are pretty equal, and then the variation of the last quantile is super high, which would contribute to high skewing on the right. 

# let's split up the sample into separate quantile_dfs and run topic analysis on them ...

quantile1_df <- sent_df %>%
  filter(word_count < 15) %>%
  select("doc ID" = element_id, text) %>%
  mutate(label = NA)
quantile1_df <- quantile1_df[,c(1,3,2)] %>%
  fwrite("output/plushcare_q1.csv")

quantile2_df <- sent_df %>%
  filter(word_count >= 15 & word_count < 31) %>%
  select("doc ID" = element_id, text) %>%
  mutate(label = NA)
quantile2_df <- quantile2_df[,c(1,3,2)] %>%
  fwrite("output/plushcare_q2.csv")

quantile3_df <- sent_df %>%
  filter(word_count >= 31 & word_count < 61) %>%
  select("doc ID" = element_id, text) %>%
  mutate(label = NA)
quantile1_df <- quantile3_df[,c(1,3,2)] %>%
  fwrite("output/plushcare_q3.csv")

quantile4_df <- sent_df %>%
  filter(word_count >= 61)  %>%
  select("doc ID" = element_id, text) %>%
  mutate(label = NA)
quantile4_df <- quantile4_df[,c(1,3,2)] %>%
  fwrite("output/plushcare_q4.tsv", sep = "\t")



# NEXT STOP: try unsupervised text clustering

# https://medium.com/@rohithramesh1991/unsupervised-text-clustering-using-natural-language-processing-nlp-1a8bc18b048d

fwrite(quantile1_df,  "output/plushcare_sa.csv")

# Next: is there any relationship between word count, rating, and sentiment?

df <- isolate_quants(sent_df) %>%
  select(-c(element_id, sd))
plot_quants("output/plushcare_corrs.png",df)

# Hm, seems to be some relationships, possibly. 

# can rating & word count predict sentiment? (use linear regression with dummy variables)
