### Load Libraries
library(tidytext)
library(tidyverse)

### Load Data
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

### Clean data
## Quickly correct some categories
boston_cocktails <- boston_cocktails %>% 
  mutate(category = if_else(category == 'Rum - Daiquiris', 'Rum', category))

## Remove infrequent categories
category_count <- boston_cocktails %>% 
  group_by(category) %>%
  count() %>%
  filter(n >= 100)

cleaned_cocktails <- boston_cocktails %>%
  filter(category %in% category_count$category)


## Properly transform measure from str to int
cleaned_cocktails <- cleaned_cocktails %>% 
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
  select(name, category, ingredient_number, ingredient, measure)

## Divide ingredients into words
top_words <- cleaned_cocktails %>%
  distinct(ingredient) %>% 
  unnest_tokens(word, ingredient) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  count() %>%
  filter(n > quantile(.$n, 0.8))

## Create column for words
for (ix in c(1:nrow(top_words))){
  word <- top_words$word[ix]
  cleaned_cocktails[, word] <- map(cleaned_cocktails$ingredient, str_detect, word) %>% flatten_int()
  cleaned_cocktails[, word] <- cleaned_cocktails[, word] * cleaned_cocktails[, 'measure']
}

## Final clean & format (1 row by cocktail)
cleaned_cocktails <- cleaned_cocktails %>%
  select(- ingredient_number, - ingredient, -measure, # remove obsolete non-numerical values
         -`1`, - oz, - slice, - wheel) %>% # remove non-informative columns
  group_by(name, category) %>%
  summarise_all(sum) # get sum of word frequency by name + category

### KNN Clustering
## Create scaled df
scaled_df <- cleaned_cocktails %>% 
  ungroup() %>%
  select(-name, -category) %>% 
  apply(2, scale)

## Remove all NaN
scaled_df <- scaled_df[ , colSums(is.na(scaled_df)) < nrow(scaled_df)]

## KNN Clustering Proper
max_k <- cleaned_cocktails %>% ungroup() %>% distinct(category) %>% nrow()

knn <- kmeans(scaled_df, centers = max_k, nstart = 25)
cleaned_cocktails$cluster <- knn$cluster

### PCA
## Get results into df
pca_result <- prcomp(scaled_df)
pca_df <- predict(pca_result, newdata = scaled_df) %>% as_data_frame()

pca_df$name <- cleaned_cocktails$name
pca_df$category <- cleaned_cocktails$category
pca_df$cluster <- as_factor(cleaned_cocktails$cluster)

### Plot
pca_df %>%
  ggplot(aes(PC1, PC2, color = cluster)) + 
  geom_point() +
  facet_wrap(~ category) +
  theme_bw()




pca_df %>%
  group_by(cluster) %>% 
  count()
