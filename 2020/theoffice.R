library("schrute")
llibrary(tidyverse)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
office_transcripts <- schrute::theoffice

office_ratings %>% head()
office_transcripts %>% head()


###############
office_ratings %>% 
  ggplot(aes(episode, imdb_rating)) +
  geom_path() +
  facet_grid(~ season)


### THE FINALE EFFECT
office_ratings %>% 
  ggplot(aes(episode, imdb_rating)) +
  geom_point()


##############3

character_text <- office_transcripts %>%
  mutate(length_lines = str_count(text),
         season = as.numeric(season),
         episode = as.numeric(episode)) %>%
  group_by(season, episode, character) %>%
  summarise(n_lines = n(),
            mean_length_lines = mean(length_lines),
            max_length_lines = max(length_lines))

character_count <- office_transcripts %>%
  group_by(character) %>%
  count() %>% 
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(5)

office_ratings %>%
  left_join(character_text, by = c("season", "episode")) %>%
  filter(character %in% character_count$character) %>%
  filter(imdb_rating > 9) %>%
  ggplot(aes(n_lines, imdb_rating, color = character)) + 
  geom_point() +
  facet_grid(~ character)

office_ratings %>%
  left_join(character_text, by = c("season", "episode")) %>%
  filter(character %in% character_count$character) %>%
  ggplot(aes(max_length_lines, imdb_rating, color = character)) +
  geom_jitter()


test <- office_ratings %>%
  left_join(character_text, by = c("season", "episode")) %>% 
  spread(character, line_val)

reg <- lm(imdb_rating ~ season + episode + character * n_lines + character * mean_length_lines +
     max_length_lines, data = test)
summary(reg)