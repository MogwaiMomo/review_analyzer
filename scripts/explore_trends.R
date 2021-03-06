library(hrbrthemes)
require(PerformanceAnalytics)

# create an element_id col based on row name
create_id_var <- function(df) {
  rowid_to_column(df) %>%
    # rename rowid col to element_id
    rename(element_id = rowid) -> df
  return(df)
}

isolate_quants <- function(df) {
 df %>% 
    select_if(function(col) is.numeric(col) | is.integer(col) |
               all(col == .$element_id)) -> quants
   return(quants)
}
 
isolate_quals <- function(df) {
  df %>% 
    select_if(function(col) is.factor(col) | 
                all(col == .$element_id)) -> quals
  return(quals)
}

isolate_texts <- function(df) {
  df %>% 
    select_if(function(col) is.character(col) | 
                all(col == .$element_id)) -> texts
  return(texts)
}

   
# 3. Graph & explore variables

## quant plots
plot_quants <- function(file, df, omit_var=NULL) {
  png(filename=file, width = 1480, height = 1480)
  if (!is.null(omit_var)) {
    df %>%
      select(-all_of(omit_var)) -> df
  }
  plot <- chart.Correlation(df,hist=T)
  dev.off()
}


# explore categories by box plot
plot_boxplot <- function(df, factor_var, num_var) {
  factor_var <- sym(factor_var)
  num_var <- sym(num_var)
  
  df %>%
    mutate(num_var = !!num_var) %>%
    mutate(factor_var = !!factor_var) %>%
    group_by(factor_var) -> grouped_df
  
  plot <- grouped_df %>%
    ggplot(aes(x=factor_var, y=num_var)) +
    geom_boxplot()
}
