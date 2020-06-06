library(tidytext)
library(tidyverse)

# Load Data
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

# Clean data
## Properly transform measure from str to int
cleaned_cocktails <- boston_cocktails %>% 
  separate(measure, into = c('measure', 'unit'), sep = ' ') %>% # should get last instead
  mutate(
    measure = case_when(
      measure == '1/2' ~ 0.5,
      measure == '1/4' ~ 0.25,
      T ~ as.numeric(measure)
    )
  ) %>%
  filter(!is.na(measure)) %>%
  mutate(
    measure = case_when(
      unit == 'oz' ~ measure,
      unit == 'dash' ~ measure / 100,
      unit == 'splash' ~ measure / 10,
      unit == 'glass' ~ 4 * measure,
      unit == 'bottles' ~ 10 * measure,
      T ~ 0
    )
  ) %>%
  filter(measure != 0 & !is.na(measure)) 

## Clean ingredients field
cleaned_cocktails <- cleaned_cocktails %>%
  mutate(
    ingredient = case_when(
        ingredient == 'Juice of Orange' ~ 'Orange Juice',
        ingredient == 'Juice of a Lime' ~ 'Lime Juice',
        T ~ ingredient
        )
  ) %>%
  select(category, ingredient_number, ingredient, measure)

# Divide ingredients into words
top_words <- cleaned_cocktails %>%
  distinct(ingredient) %>% 
  unnest_tokens(word, ingredient) %>%
  group_by(word) %>%
  count() %>%
  filter(n > quantile(.$n, 0.8))

# Create column for words
for (ix in c(1:nrow(top_words))){
  word <- top_words$word[ix]
  cleaned_cocktails[, word] <- map(cleaned_cocktails$ingredient, str_detect, word) %>% flatten_int()
}

# Create scaled df
scaled_df <- cleaned_cocktails %>% 
  select(-category, -ingredient) %>% 
  apply(2, scale)

# remove all NaN
scaled_df <- scaled_df[ , colSums(is.na(scaled_df)) < nrow(scaled_df)]


pca_result <- prcomp(scaled_df)
biplot(pca_result, scale = 0)

# knn
max_k <- cleaned_cocktails %>% distinct(category) %>% nrow()

knn <- kmeans(scaled_df, centers = max_k, nstart = 25)
cleaned_cocktails$cluster <- knn$cluster


cleaned_cocktails %>%
  group_by(cluster)

