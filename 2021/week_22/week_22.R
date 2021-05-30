## Week 22, TidyTuesday 2021: Mario Kart World Records
## note: to make cumulative graph cleaner, should limit to 0 before 1st record


#### SETUP ####
library(tidyverse)
library(lubridate)

records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

drivers <- drivers %>% 
  mutate(records = replace_na(records, 0)) %>% 
  arrange(player, year)

### Record Times

top_tracks <- records %>% group_by(track) %>% count() %>% arrange(desc(n)) %>% head(6) %>% pull(track)


records %>%
  filter(track %in% c('Rainbow Road', "Toad's Turnpike", "Wario Stadium") & type == 'Three Lap') %>%
  ggplot(aes(date, time, color = shortcut)) +
  geom_point(alpha = 0.3) +
  labs(x='', y = 'Time (s)', color = 'Shortcut?',
       title = 'In Select Tracks, Shortcuts Drop Record Time To Under 1 Minute') +
  facet_wrap( ~ track) +
  theme_bw()

### Plot 2: Cumulative Records by Driver
top_players <- drivers %>% filter(position <= 5) %>% distinct(player) %>% pull(player)

cs_records <- drivers %>%
  group_by(player) %>% 
  summarize(cumsum_records = cumsum(records)) %>%
  pull(cumsum_records)

drivers$cumsum_records <- cs_records

cols <- c("#003399", 
          "#FF2B4F", 
          "#fcab27", 
          "#3686d3",
          "#cf082a",
          "#f58516")

drivers %>%
  ggplot(aes(year, cumsum_records, group = player)) + 
  geom_path(data = . %>% filter(cumsum_records >= 1), alpha = 0.1) +
  geom_path(data = . %>% filter(player %in% top_players), 
            aes(color = player)) +
  scale_color_manual(values = cols) +
  labs(x='', y='Cumulative Records', color='Player',
       title = 'Mario Kart Racing Becoming More Competitive',
       subtitle = 'After years dominated by a single driver, a new crop of players are competing for #1 ranking.') +
  theme_bw()

## Difference in record between systems
records %>% 
  filter(shortcut == 'No' & type == 'Three Lap') %>% 
  group_by(track, system_played) %>% 
  summarise(time = min(time)) %>%
  ggplot(aes(fct_reorder(track, desc(time)), time, color = system_played)) +
  geom_point() +
  coord_flip() +
  scale_color_manual(values = cols) +
  labs(x='', y = 'Time (s)', color = 'System',
       title = 'PAL systems dominate Mario Kart Records') +
  theme_bw()
