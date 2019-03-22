### Tidy Tuesday: Week 11, 2019
# Load Libraries
library(tidyverse)
library(ggrepel)

# Load Data
board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

# Trim Dataframe
board_games <- board_games %>% filter(users_rated >= quantile(board_games$users_rated, 0.20)) %>%
  select(name, max_players, max_playtime, min_age, min_players, min_playtime, playing_time,
         year_published, average_rating)

# Replicating FiveThirtyEight Plots
## from https://fivethirtyeight.com/features/designing-the-best-board-game-on-the-planet/
key_games <- c("Acquire", "Connect Four", "Mouse Trap", "The Settlers of Catan", "Twilight Struggle")

## Rating Per Year
ggplot(board_games, aes(year_published, average_rating, label = name))+ 
  geom_jitter(alpha = 0.05) + 
  geom_smooth(se = FALSE, color = "red") +
  geom_jitter(data = filter(board_games,  name %in% key_games)) + 
  geom_text_repel(data = filter(board_games,  name %in% key_games)) +
  ylim(c(0, 10)) +
  labs(title = "A Golden Age of Board Games?", 
       subtitle = "Average User Rating by Original Year of Production",
    x = "", y = "Average Rating", caption = "Source: Board Game Geek")

## Max Players
board_games %>% group_by(max_players) %>% summarize(average_rating = mean(average_rating)) %>%
  filter(max_players >= 2 & max_players <= 8) %>% 
  ggplot(aes(max_players, average_rating)) + geom_line() + geom_point() +
  labs(title = "Three Players Might Be A Sweet Spot", 
       subtitle = "Average User Rating by Maximum Number of Players",
       x = "Max Players", y = "Average Rating", caption = "Source: Board Game Geek")

#note: as it turns out, single player games have a higher rating!
#there are 105 such games in the dataframe, with an av. rating of approx 7
#board_games %>% filter(max_players == 1) %>% count()

board_games %>% group_by(max_players) %>% summarize(average_rating = mean(average_rating)) %>%
  filter(max_players >= 1 & max_players <= 8) %>% 
  ggplot(aes(max_players, average_rating)) + geom_line() + geom_point() +
  labs(title = "Riding Solo", 
       subtitle = "Highest Ranked Games are Solitaire Games",
       x = "Max Players", y = "Average Rating", caption = "Source: Board Game Geek")

## Average Play Time
board_games %>% filter(playing_time <= 6*60 ) %>% 
  ggplot(aes(playing_time, average_rating, label = name)) + 
  geom_jitter(alpha = 0.01, width = 0.5) + 
  geom_smooth(se = FALSE, color = "red") +
  geom_jitter(data = filter(board_games,  name %in% key_games), width = 0.5) + 
  geom_text_repel(data = filter(board_games,  name %in% key_games)) +
  labs(title = "Longer Is (Sometimes) Better", 
       subtitle = "Average User Rating by Average Play Time",
       x = "Average Playtime (min.)", y = "Average Rating", caption = "Source: Board Game Geek")


