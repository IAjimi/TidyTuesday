## LOADING LIBRARIES
library(ggrepel)
library(tidyverse)

## IMPORTING DATA
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

## TOP 3 SHOWS BY SEATS SOLD LAST YEAR
top_show <- grosses %>%
  filter(week_ending >= '2019-01-01' & week_ending <= '2020-01-01') %>%
  group_by(show) %>%
  summarise(tot_seats_sold = sum(seats_sold))  %>%
  ungroup() %>%
  top_n(3)

## % CAPACITY OF TOP 3 SHOWS 
grosses %>%
  filter(week_ending >= '2013-01-01' & show %in% top_show$show) %>%
  group_by(show, week_ending, week_number) %>%
  summarise(pct_capacity = mean(pct_capacity, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(week(week_ending), pct_capacity, group = year(week_ending))) +
  geom_path(data = . %>% filter(week_ending < '2020-01-01'), alpha = 0.15) +
  geom_path(data = . %>% filter(week_ending >= '2020-01-01'), color = '#c70039', size = 0.715) +
  geom_point(data = . %>% filter(week_ending == max(week_ending)), color = '#c70039', size = 1) +
  geom_text_repel(data = . %>% filter(week_ending == max(week_ending) & show == 'Aladdin'), 
                  aes(label = '2020 % capacity before \n COVID-19 closings'), 
                  nudge_x = 2, nudge_y = -0.085, color = '#c70039') +
  scale_y_continuous(labels = percent) +
  labs(x = 'Week', y = '% Capacity') +
  facet_grid(row = vars(show)) +
  theme_bw()

## # SEATS SOLD
grosses %>%
  filter(week_ending >= '2013-01-01' & show %in% top_show$show) %>%
  ggplot(aes(week(week_ending), seats_sold, group = year(week_ending))) +
  geom_path(data = . %>% filter(week_ending < '2020-01-01'), alpha = 0.15) +
  geom_path(data = . %>% filter(week_ending >= '2020-01-01'), color = '#c70039', size = 0.715) +
  geom_point(data = . %>% filter(week_ending == max(week_ending)), color = '#c70039', size = 1) +
  geom_text_repel(data = . %>% filter(week_ending == max(week_ending) & show == 'Aladdin'), 
                  aes(label = '2020 seat sales before \n COVID-19 closings'), 
                  nudge_x = 3, nudge_y = -2000, color = '#c70039') +
  labs(x = 'Week', y = 'Total Seats Sold per Week') +
  facet_grid(row = vars(show)) +
  theme_bw()

## SAME GRAPH w/ AVG PRICE
grosses %>%
  filter(week_ending >= '2013-01-01' & show %in% top_show$show) %>%
  ggplot(aes(week(week_ending), avg_ticket_price, group = year(week_ending))) +
  geom_path(data = . %>% filter(week_ending < '2020-01-01'), alpha = 0.15) +
  geom_path(data = . %>% filter(week_ending >= '2020-01-01'), color = '#c70039', size = 0.715) +
  geom_point(data = . %>% filter(week_ending == max(week_ending)), color = '#c70039', size = 1) +
  geom_text_repel(data = . %>% filter(week_ending == max(week_ending) & show == 'Aladdin'), 
                  aes(label = '2020 before COVID-19 closings'), 
                  nudge_x = 2, nudge_y = -10, color = '#c70039') +
  labs(x = 'Week', y = 'Avg Ticket Price ($)') +
  facet_grid(row = vars(show)) +
  theme_bw()
