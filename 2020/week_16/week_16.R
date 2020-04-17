## LOADING LIBRARIES
library(plotly)
library(genius)
library(tidyverse)
library(tidytext)

## IMPORTING DATA
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
           artist == 'Wu-Tan Clan' ~ 'Wu-Tang Clan',
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
  filter(n > 1) %>%
  ggplot(aes(n, points, label = Artist)) +
  geom_jitter(data = . %>% filter(!artist %in% top_artist_decade$artist), size = 2, alpha = 0.5) +
  geom_point(data = . %>% filter(artist %in% top_artist_decade$artist), 
             color = '#FF2B4F', size = 2) +
  geom_text(data = . %>% filter(artist %in% top_artist_decade$artist), aes(label = artist), 
            nudge_x = .7, nudge_y = 5, color = '#FF2B4F', size = 4) +
  geom_text(data = . %>% filter(artist == 'JAY-Z'), aes(label = artist), 
            nudge_y = 7,  size = 4) +
  labs(x = 'Number of Songs', y = 'Total Points', title = 'Best Rappers, Ranked') +
  theme_minimal()

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


## SPECIFIC SENTIMENT 
p4 <- artist_lyrics %>% 
  mutate(lyric = str_to_lower(lyric)) %>%
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words, by = "word") %>%
  left_join(., get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c('anger', 'joy', 'sadness', 'trust')) %>%
  filter(!word %in% c('black', 'money', 'white', 'police', 'hood', 'cop', 'bout')) %>% #incorrect associations
  group_by(artist, track, sentiment) %>%
  count() %>%
  group_by(artist, sentiment) %>%
  summarize(mean_sentiment = mean(n, na.rm = T)) %>%
  inner_join(top_artist, by = "artist") %>%
  spread(sentiment, mean_sentiment) %>%
  mutate(Artist = paste(artist, '\n',
                        'Anger: ', round(anger, 0), '\n',
                        'Trust: ', round(trust, 0), '\n',
                        'Total Points: ', points, '\n',
                        sep = '')
  ) %>%
  ggplot(aes(anger, trust, label = Artist, size = points, color = points)) +
  geom_smooth(method = 'lm', se = F, color = '#ff0505') +
  geom_point(alpha = 0.6) +
  geom_point(data = . %>% filter(artist %in% top_artist_decade$artist), 
             color = '#FF2B4F') +
  geom_text(data = . %>% filter(artist %in% top_artist_decade$artist), aes(label = artist), 
            nudge_y = 2, color = '#FF2B4F', size = 4) +
  scale_color_continuous(low = '#ffe6b3', high = '#ff0505') +
  guides(color = F, size = F) +
  labs(x = 'Anger', y = 'Trust', title = 'Average Count of Words per Sentiment') +
  theme_minimal()

p4 %>%
  ggplotly(tooltip = 'Artist')

