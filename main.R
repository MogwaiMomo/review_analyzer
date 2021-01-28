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
f <- "output/plushcare_reviews.csv"
data <- fread(f)

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

fwrite(quantile1_df,  "output/plushcare_sa.csv")

# Next: is there any relationship between word count, rating, and sentiment?

df <- isolate_quants(sent_df) %>%
  select(-c(element_id, sd))
plot_quants("output/plushcare_corrs.png",df)

# Hm, seems to be some relationships, possibly. 

# can rating & word count predict sentiment? (use linear regression with dummy variables)

lm.fit <- lm(ave_sentiment ~ word_count + rating, data = sent_df)
summary(lm.fit)

# according to this model, word count & particularly low & high ratings (1, 4, 5) predict sentiment scores. Word count has a slightly negative relationship, ratings has a positive relationship.

# Note that the Rsquared value is low, meaning that this model doesn't account for much of the variation in the sample. So it's not a great fit. 

# Does this model fit our assumptions? Need to check our residuals ...
run_residual_plots <- function(fit_var, output_file) {
  png(filename=output_file,
      width=1500,
      height=1000,
      units="px",
      res=140)
  
  par(mfrow=c(2,2)) 
  plot(fit_var)
  dev.off()
}
  

# Plot 1: Residuals vs. Fitted
# Look for a random, even cloud. If you see curves, this indicates a non-linear relationship between predictor & outcome. Likely a transformation (start with Log) may be required. Clustering indicates a missing variable. 
# Try  log transformation (of both the predictor and the response) to remove curve & clustering

# transform just the predictor first:
log.lm.fit <- lm(ave_sentiment ~ log(word_count) + rating, 
                 data = sent_df)
summary(log.lm.fit)
run_residual_plots(log.lm.fit, "output/plushcare_res.png")

# need to omit rows with ave_sentiment = 0 before doing a log transform on it. (How many are there?)

sent_df_no_zeros <- sent_df %>%
  filter(ave_sentiment != 0)

# log-transform both predictor and response this time

log.lm.fit <- lm(ave_sentiment ~ log(word_count) + rating, 
                 data = sent_df)
summary(log.lm.fit)
run_residual_plots(log.lm.fit, "output/plushcare_res.png")

# Plot 2: Normal Q-Q Plot
# If things generally follow a straight line with higher density in the middle, that's good. you may see outliers that have high leverage. This is comparing the distribution of your residuals (which should be normal) to the residuals of a theoretical (normal sample). S-shaped curves denote extreme values/higher than normal variance, and concave/convex curves denote skewed distributions. 

# Plot 3: Scale-Location Plot 
# It’s also called Spread-Location plot. This plot shows if residuals are spread equally along the ranges of predictors. This is how you can check the assumption of equal variance (homoscedasticity). It’s good if you see a horizontal line with equally (randomly) spread points.

# Plot 4: Residuals vs Leverage
# Unlike the other plots, this time patterns are not relevant. We watch out for outlying values at the upper right corner or at the lower right corner. Those spots are the places where cases can be influential against a regression line. Look for cases outside of a dashed line, Cook’s distance. When cases are outside of the Cook’s distance (meaning they have high Cook’s distance scores), the cases are influential to the regression results. The regression results will be altered if we exclude those cases.






