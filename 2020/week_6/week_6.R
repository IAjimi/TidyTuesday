attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

attendance %>% head()
standings %>% head()
games %>% head()

# What type of team has highest attendance?
lead_games <- games %>% mutate(week = as.numeric(week) + 1)

weekly_attendance_df <- attendance %>% 
  select(-total, - home, -away) %>%
  left_join(lead_games)

weekly_attendance_df %>%
  ggplot(aes(weekly_attendance, pts_win, color = team == "New York")) + geom_point() + guides(Color = FALSE)



# Los Angeles
# Dallas, New York
attendance %>%  
  filter(team == "Los Angeles") %>%
  ggplot(aes(week, weekly_attendance)) +
  geom_point() +
  facet_grid(~ year)

standings <- mutate(standings, full_team_name = paste(team, team_name))

path_to_victory <- games %>%
  left_join(standings, by = c("home_team" = "full_team_name", "year")) %>%
  mutate(game_week = case_when(
    week == "WildCard" ~ 18,
    week == "Division" ~ 19,
    week == "ConfChamp" ~ 20,
    week == "SuperBowl" ~ 21,
    T ~ as.numeric(week)
  )) %>%
  mutate(pts_home_team = if_else(home_team == winner, pts_win - pts_loss, pts_loss - pts_win))

path_to_victory %>%
  filter(year >= 2015) %>%
  ggplot(aes(game_week, pts_home_team, group = team)) +
  geom_path(data = . %>% filter(sb_winner != "Won Superbowl"), alpha = 0.5, color = "darkgrey") +
  geom_path(data = . %>% filter(sb_winner == "Won Superbowl"), size = 1, alpha = 0.8, color = "red") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  guides(color = FALSE) +
  labs(x = "Week", y = "Points Differential",
       title = "Path to Victory", subtitle = "Point Differential for Both Superbowl Teams By Season") +
  facet_grid(~ year)
