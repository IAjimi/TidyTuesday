
library(tidyverse)

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

## date, race, site are unique combos

## is there a home advantage for marbles?
marbles %>%
  split(.$race) %>%
  map(arrange, time_s) %>%
  map(mutate, place = dense_rank(time_s))


## points
marbles %>%
  group_by(host) %>%
  summarize(mean_points = mean(points, na.rm = T))


## marble with most points
best_marble <- marbles %>%
  group_by(marble_name) %>%
  summarise(points = sum(points, na.rm = T),
            number_races = n()) %>%
  mutate(points_per_race = points / number_races) %>%
  arrange(desc(points))

best_marble %>%
  ggplot(aes(fct_reorder(marble_name, points), points)) +
  geom_col() +
  coord_flip() +
  theme_bw()

marbles %>% select(notes) %>% filter(!is.na(notes))

            