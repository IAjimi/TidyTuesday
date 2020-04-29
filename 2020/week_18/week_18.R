## LOADING LIBRARIES
library(ggrepel)
library(tidyverse)

## IMPORTING DATA
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')

## ADJUSTING PRICES FOR INFLATION
# Adding dates
grosses <- grosses %>%
  mutate(year_ = year(week_ending),
         month_ = month(week_ending),
         week_ = week(week_ending))

cpi <- cpi %>%
  mutate(year_ = year(year_month),
         month_ = month(year_month))

# Finding latest CPI value
latest_cpi <- cpi %>% filter(year_month == max(year_month))

# Joining and computing adjusted average ticket prices
grosses <- grosses %>% 
  left_join(cpi, by = c("year_", "month_")) %>%
  mutate(avg_ticket_price = avg_ticket_price * latest_cpi$cpi / cpi)

## TOP 3 SHOWS BY SEATS SOLD LAST YEAR
top_show <- grosses %>%
  filter(week_ending >= '2019-01-01' & week_ending <= '2020-01-01') %>%
  group_by(show) %>%
  summarise(tot_seats_sold = sum(seats_sold))  %>%
  ungroup() %>%
  top_n(3, tot_seats_sold)

## % CAPACITY OF TOP 3 SHOWS 
grosses %>%
  filter(week_ending >= '2013-01-01' & show %in% top_show$show) %>%
  ggplot(aes(week_, pct_capacity, group = year(week_ending))) +
  geom_path(data = . %>% filter(week_ending < '2020-01-01'), alpha = 0.15) +
  geom_path(data = . %>% filter(week_ending >= '2020-01-01'), color = '#c70039', size = 0.715) +
  geom_point(data = . %>% filter(week_ending == max(week_ending)), color = '#c70039', size = 1) +
  geom_text_repel(data = . %>% filter(week_ending == max(week_ending) & show == 'Aladdin'), 
                  aes(label = '2020 before \n COVID-19 closings'), 
                  nudge_x = 1, nudge_y = -0.075, color = '#c70039') +
  scale_y_continuous(labels = percent) +
  labs(x = 'Week', y = '% Capacity') +
  facet_grid(row = vars(show)) +
  theme_bw()

## # SEATS SOLD
grosses %>%
  filter(week_ending >= '2013-01-01' & show %in% top_show$show) %>%
  ggplot(aes(week_, seats_sold, group = year(week_ending))) +
  geom_path(data = . %>% filter(week_ending < '2020-01-01'), alpha = 0.15) +
  geom_path(data = . %>% filter(week_ending >= '2020-01-01'), color = '#c70039', size = 0.715) +
  geom_point(data = . %>% filter(week_ending == max(week_ending)), color = '#c70039', size = 1) +
  geom_text_repel(data = . %>% filter(week_ending == max(week_ending) & show == 'Aladdin'), 
                  aes(label = '2020'), 
                  nudge_x = 1, nudge_y = -500, color = '#c70039') +
  labs(x = 'Week', y = 'Total Seats Sold per Week') +
  facet_grid(row = vars(show)) +
  theme_bw()

## SAME GRAPH w/ AVG PRICE
grosses %>%
  filter(week_ending >= '2013-01-01' & show %in% top_show$show) %>%
  ggplot(aes(week_, avg_ticket_price, group = year(week_ending))) +
  geom_path(data = . %>% filter(week_ending < '2020-01-01'), alpha = 0.15) +
  geom_path(data = . %>% filter(week_ending >= '2020-01-01'), color = '#c70039', size = 0.715) +
  geom_point(data = . %>% filter(week_ending == max(week_ending)), color = '#c70039', size = 1) +
  geom_text_repel(data = . %>% filter(week_ending == max(week_ending) & show == 'Aladdin'), 
                  aes(label = '2020'), 
                  nudge_x = 1, nudge_y = -10, color = '#c70039') +
  labs(x = 'Week', y = 'Avg Ticket Price ($)') +
  facet_grid(row = vars(show)) +
  theme_bw()

## SEASONALITY IN AVG TICKET PRICES
# Finding which weeks had highest avg_prices
label_df <- grosses %>%
  filter(week_ending >= '2013-01-01' & show == 'Aladdin') %>%
  group_by(week_, show) %>%
  summarise(avg_ticket_price = max(avg_ticket_price)) %>%
  ungroup() %>%
  top_n(8) %>%
  mutate(
    season_label = case_when(
      week_ == 53 ~ 'Xmas & NYE',
      week_ == 48 ~ 'Thanksgiving',
      week_ == 16 ~ 'Easter',
      T ~ NA_character_
    )
  ) %>%
  filter(!is.na(season_label)) %>%
  left_join(mutate(grosses, week_ = week(week_ending)), by = c("week_", "show", "avg_ticket_price"))

grosses %>%
  filter(week_ending >= '2013-01-01' & show %in% top_show$show) %>%
  mutate(
    season_label = case_when(
      week_ %in% c(52, 53) ~ 'Xmas & NYE',
      week_ == 48 ~ 'Thanksgiving',
      week_ == 16 ~ 'Easter',
      T ~ NA_character_
    )
  )  %>%
  ggplot(aes(week_, avg_ticket_price, group = year(week_ending))) +
  geom_path(alpha = 0.15) +
  geom_point(data = label_df, color = '#c70039') +
  geom_text_repel(data = label_df,  aes(label = season_label), 
                  nudge_y = 25, color = '#c70039') +
  labs(x = 'Week', y = 'Avg Ticket Price ($)', 
       title = 'Seasonality in Broadway Ticket Pricing',
       subtitle = 'Dynamic pricing leads to peaks around holidays (Easter, Thanksgiving, NYE), up to twice the usual price',
       caption = 'Prices adjusted for inflation.') +
  facet_grid(row = vars(show)) +
  theme_bw()
