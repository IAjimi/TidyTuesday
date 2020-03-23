library(tidyverse)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

attendance %>% head()
standings %>% head()
games %>% head()

# Attendance before/after superbowl win
sb_winner <- standings %>% 
  select(team_name, year, sb_winner) %>%
  filter(sb_winner == "Won Superbowl")

sb_winner_year_after <- sb_winner %>%
  mutate(year = year + 1,
         sb_winner = NA)

sb_before_after <- sb_winner %>% 
  full_join(sb_winner_year_after,  by = c("team_name", "year")) %>%
  mutate(
         sb_winner.x = if_else(is.na(sb_winner.x), "No Superbowl", sb_winner.x),
         sb_winner.y = if_else(is.na(sb_winner.y), "No Superbowl", sb_winner.x)
         ) %>%
  rename(sb_winner = sb_winner.x, sb_winner_year_after = sb_winner.y) %>%
  left_join(attendance,  by = c("team_name", "year")) %>%
  mutate(weeks_before_after_sb = if_else(sb_winner == "No Superbowl", week, week - 17))
  
sb_before_after_baseline <- sb_before_after %>%
  filter(sb_winner == "Won Superbowl" & week <= 12) %>%
  group_by(team_name) %>%
  summarize(mean_attendance = mean(weekly_attendance, na.rm = T),
            sd_attendance = sd(weekly_attendance, na.rm = T))

reg <- lm(weekly_attendance ~ team_name * year, data = sb_before_after)
  
sb_before_after %>%
  filter(!is.na(weekly_attendance)) %>%
  left_join(sb_before_after_baseline, by = "team_name") %>% 
  mutate(normed_attendance = (weekly_attendance - mean_attendance) / sd_attendance,
         resids = resid(reg, newdata = sb_before_after)) %>%
  group_by(weeks_before_after_sb) %>%
  summarise(weekly_attendance = mean(weekly_attendance, na.rm = T)) %>%
  ggplot(aes(weeks_before_after_sb, weekly_attendance)) +
  geom_path() +
  geom_vline(xintercept = 0) +
  labs(x = "Weeks Before/After the Superbowl", y = "Mean Weekly Attendance",
       title = "No Superbowl Boost in Attendance for Winners")

#### ALT: superbowl winners are already more popular than other teams
attendance %>%
  anti_join(sb_before_after, by = c("team_name", "year")) %>%
  mutate(won_once = if_else(team_name %in% sb_winner$team_name, "Yes", "No")) %>%
  ggplot(aes(won_once, weekly_attendance)) + geom_boxplot() +
  labs(x = "Won the Superbowl Once Since 2000", y = "Weekly Attendance",
       title = "No Superbowl Boost in Attendance for Winners")

attendance %>%
  group_by(team, team_name) %>%
  summarise(weekly_attendance = mean(weekly_attendance, na.rm = T)) %>%
  mutate(won_once = if_else(team_name %in% sb_winner$team_name, "Yes", "No")) %>%
  arrange(desc(weekly_attendance))

attendance %>%
  group_by(team, team_name, year) %>%
  summarise(weekly_attendance = mean(weekly_attendance, na.rm = T)) %>%
  mutate(won_once = if_else(team_name %in% sb_winner$team_name, "Yes", "No")) %>%
  left_join(sb_winner, by = c("team_name", "year")) %>%
  ggplot(aes(year, weekly_attendance, group = team_name)) +
  geom_path(color = "grey") +
  #geom_path(data = . %>% filter(team %in% c("Dallas", "Washington", "New York", "Los Angeles",
  #                                          "Arizona")), aes(color = team_name), size = 1) +
  geom_path(data = . %>% filter(won_once == "Yes"), aes(color = team_name), size = 1) +
  geom_point(data = . %>% filter(sb_winner == "Won Superbowl"), aes(color = team_name), size = 3) +
  labs(x = "", y = "Mean Weekly Attendance")


#####

attendance %>% head()
standings %>% head()
games %>% head()


# What type of team has highest attendance?
lead_games <- games %>% mutate(week = as.numeric(week) + 1)

weekly_attendance_df <- attendance %>% 
  select(-total, - home, -away) %>%
  left_join(lead_games)

weekly_attendance_df %>%
  group_by(team, year, day, date) %>%
  summarise(pts_win = mean(pts_win, na.rm = T),
            weekly_attendance = mean(weekly_attendance, na.rm = T)) %>%
  ggplot(aes(weekly_attendance, lag(pts_win), color = (team == "New York"))) + 
  geom_point() + 
  guides(Color = FALSE)



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
  mutate(pts_differential = if_else(home_team == winner, pts_win - pts_loss, pts_loss - pts_win))

superbowl_teams <- path_to_victory %>%
  filter(week == "SuperBowl") %>%
  mutate(superbowl_bool = T) %>%
  select(home_team, away_team, year, superbowl_bool) %>%
  gather(team_key, team_value, c(home_team, away_team))

path_to_victory %>%
  left_join(superbowl_teams, by = c("home_team" = "team_value", "year")) %>%
  filter(year >= 2015) %>%
  ggplot(aes(game_week, pts_differential , group = team)) +
  geom_path(data = . %>% filter(sb_winner != "Won Superbowl"), alpha = 0.5, color = "darkgrey") +
  geom_path(data = . %>% filter(superbowl_bool == T), size = 1, alpha = 0.8, color = "#FF2B4F") +
  geom_path(data = . %>% filter(sb_winner == "Won Superbowl"), size = 1, alpha = 0.8, color = "#3686d3") +
  geom_hline(yintercept = 0, alpha = 0.5) + 
  guides(color = FALSE) +
  labs(x = "Week", y = "Points Differential",
       title = "Path to Victory", subtitle = "Point Differential for Both Superbowl Teams By Season") +
  facet_grid(~ year)
