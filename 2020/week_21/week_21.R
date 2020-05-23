library(tidyverse)

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)
vb_matches %>% head()

## Doubles
attack_gap <- vb_matches %>%
  filter(!is.na(w_p1_tot_attacks) & !is.na(w_p2_tot_attacks)) %>%
  group_by(tournament, year, date, gender) %>%
  summarise(
         winner_min_attack = min(w_p1_tot_attacks, w_p2_tot_attacks),
         winner_max_attack = max(w_p1_tot_attacks, w_p2_tot_attacks),
         loser_min_attack = min(l_p1_tot_attacks, l_p2_tot_attacks),
         loser_max_attack = max(l_p1_tot_attacks, l_p2_tot_attacks)) %>%
  ungroup() %>%
  mutate(winner_attack_gap = winner_max_attack - winner_min_attack,
         loser_attack_gap = loser_max_attack - loser_min_attack)

attack_gap %>%
  group_by(year, gender) %>%
  summarise_if(is.numeric, mean) %>%
  ggplot(aes(year, winner_max_attack - loser_max_attack, color = gender)) +
  geom_path()


## BY PLAYER
top_players_kill <- vb_matches %>%
  filter(!is.na(w_p1_tot_kills)) %>%
  group_by(w_player1) %>%
  summarize(w_p1_tot_kills = mean(w_p1_tot_kills, na.rm = T),
            n_matches = n()) %>%
  arrange(desc(w_p1_tot_kills)) %>%
  filter(n_matches > mean(n_matches)) %>%
  ungroup() %>%
  top_n(4, w_p1_tot_kills)

top_players_kill <- vb_matches %>%
  filter(!is.na(w_p1_tot_errors)) %>%
  group_by(w_player1) %>%
  summarize(w_p1_tot_kills = mean(w_p1_tot_kills, na.rm = T),
            n_matches = n()) %>%
  arrange(desc(w_p1_tot_kills)) %>%
  filter(n_matches > mean(n_matches)) %>%
  ungroup() %>%
  top_n(4, w_p1_tot_kills)

###
top_players_kill <- vb_matches %>%
  #filter(w_player1 %in% top_players_kill$w_player1) %>% #| w_player2 %in% top_players_kill$w_player1)
  group_by(w_player1) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  top_n(4, w_p1_tot_kills)

vb_matches %>% #| w_player2 %in% top_players_kill$w_player1)
  group_by(w_player1, year) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ggplot(aes(w_p1_tot_attacks,  w_p1_tot_kills)) +
  geom_jitter(alpha = 0.2) +
  geom_jitter(data = .%>% filter(w_player1 %in% top_players_kill$w_player1),
              aes(color = w_player1), size = 1.5) +
  geom_smooth(se = F, method = 'lm')

vb_matches %>% 
  group_by(w_player1, year) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ggplot(aes(year,  w_p1_tot_kills)) +
  geom_path(alpha = 0.5) 
