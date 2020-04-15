library(plotly)
library(genius)
library(tidyverse)
library(tidytext)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

## CLEAN UP RANKINGS' ARTISTS' NAMES
rankings <- rankings %>% 
  mutate(artist = str_split(artist, 'ft.') %>% map(first) %>% flatten_chr(),
         artist = str_replace(artist, ' $', ''),
         artist = case_when(
           artist == 'Missy Elliot' ~ 'Missy Elliott',
           artist == 'Outkast' ~ 'OutKast',
           artist == 'The Notorious B.I.G' ~ 'The Notorious B.I.G.',
           artist == '2	Wu-Tan Clan' ~ '2	Wu-Tang Clan',
           T ~ artist
         )) 


## GET TOP RANKED ARTISTS
top_artist <- rankings %>% 
  group_by(artist) %>% 
  summarize(year = median(year), points = sum(points))

# To highlight on graph: top per decade
top_artist_decade <- top_artist %>%
  mutate(decade = round( (year - 1900) / 10, 0)) %>%
  split(.$decade) %>%
  map(arrange, desc(points)) %>%
  map(head, 1) %>%
  do.call('rbind', .)

# Graph
p <- top_artist %>%
  arrange(desc(points)) %>% mutate(ranking = row.names(.)) %>%
  mutate(Artist = paste(artist, '\n',
                        'Ranking: ', ranking, '\n',
                        'Total Points: ', points, '\n',
                        'Year: ', round(year, 0),
                        sep = '')
           ) %>%
  ggplot(aes(year, points, label = Artist)) +
  geom_jitter(data = . %>% filter(!artist %in% top_artist_decade$artist), size = 2, alpha = 0.5) +
  geom_point(data = . %>% filter(artist %in% top_artist_decade$artist), 
              color = '#FF2B4F', size = 2) +
  geom_text(data = . %>% filter(artist %in% top_artist_decade$artist), aes(label = artist), 
            nudge_y = 6, color = '#FF2B4F', size = 4) +
  labs(x = '', y = 'Total Points', title = 'Best Rappers, Ranked') +
  xlim(1973, 2020) +
  theme_minimal()

p %>% ggplotly(tooltip = 'Artist')

## POINTS PER SONGS
p2 <- rankings %>% 
  group_by(artist) %>%
  count() %>%
  arrange(desc(n)) %>%
  left_join(top_artist, by = 'artist') %>%
  mutate(Artist = paste(artist, '\n',
                        '# Songs: ', n, '\n',
                        'Total Points: ', points, '\n',
                        sep = '')
  ) %>%
  ggplot(aes(n, points, label = Artist)) +
  geom_jitter(data = . %>% filter(!artist %in% top_artist_decade$artist), size = 2, alpha = 0.5) +
  geom_point(data = . %>% filter(artist %in% top_artist_decade$artist), 
             color = '#FF2B4F', size = 2) +
  geom_text(data = . %>% filter(artist %in% top_artist_decade$artist), aes(label = artist), 
            nudge_x = .5, nudge_y = 5, color = '#FF2B4F', size = 4) +
  labs(x = 'Number of Songs', y = 'Total Points', title = 'Best Rappers, Ranked')

p2 %>% ggplotly(tooltip = 'Artist')

## GETTING LYRICS
artist_lyrics <- rankings %>%
  select(artist, track = title) %>%
  add_genius(artist, track, type = "lyrics")  

p3 <- artist_lyrics %>% 
  mutate(lyric = str_to_lower(lyric)) %>%
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words, by = "word") %>%
  distinct(artist, word, track) %>%
  group_by(artist, track) %>% 
  summarize(
            distinct_words = n()
            ) %>%
  group_by(artist) %>%
  summarise(mean_distinct_words = mean(distinct_words)) %>%
  inner_join(top_artist, by = "artist") %>%
  mutate(Artist = paste(artist, '\n',
                        'Distinct Words: ', round(mean_distinct_words, 0), '\n',
                        'Total Points: ', points, '\n',
                        sep = '')
  ) %>%
  ggplot(aes(mean_distinct_words, points, label = Artist)) +
  geom_jitter(data = . %>% filter(!artist %in% top_artist_decade$artist), size = 2, alpha = 0.5) +
  geom_point(data = . %>% filter(artist %in% top_artist_decade$artist), 
             color = '#FF2B4F', size = 2) +
  geom_text(data = . %>% filter(artist %in% top_artist_decade$artist), aes(label = artist), 
            nudge_y = 6, color = '#FF2B4F', size = 4) +
  labs(x = 'Distinct Words', y = 'Total Points', title = 'Best Rappers, Ranked') +
  theme_minimal()

p3 %>% ggplotly(tooltip = 'Artist')  


## MOST COMMON WORDS
artist_lyrics %>% 
  mutate(lyric = str_to_lower(lyric)) %>%
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words, by = "word") %>%
  group_by(word) %>%
  count() %>%
  filter(n <= quantile(n, 0.2))


## MEAN SENTIMENT
artist_lyrics %>% 
  mutate(lyric = str_to_lower(lyric)) %>%
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words, by = "word") %>%
  distinct(artist, word, track) %>%
  group_by(artist, track) %>% 
  summarize(
    distinct_words = n()
  ) %>%
  group_by(artist) %>%
  summarise(mean_distinct_words = mean(distinct_words)) %>%
  inner_join(top_artist, by = "artist") %>%
  mutate(Artist = paste(artist, '\n',
                        'Distinct Words: ', round(mean_distinct_words, 0), '\n',
                        'Total Points: ', points, '\n',
                        sep = '')
  ) %>%
  ggplot(aes(mean_distinct_words, points, label = Artist)) +
  geom_jitter(data = . %>% filter(!artist %in% top_artist_decade$artist), size = 2, alpha = 0.5) +
  geom_point(data = . %>% filter(artist %in% top_artist_decade$artist), 
             color = '#FF2B4F', size = 2) +
  geom_text(data = . %>% filter(artist %in% top_artist_decade$artist), aes(label = artist), 
            nudge_y = 6, color = '#FF2B4F', size = 4) +
  labs(x = 'Avg. Distinct Words per Song', y = 'Total Points', title = 'Best Rappers, Ranked') +
  theme_minimal()
