### WEEK 9, 2021 TIDYTUESDAY
library(tidyverse)

employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

#### Employment ####
### Employment by sex
## only using 2020 since most recent + not that many years in dataset
women_2020 <- employed %>% 
  filter(race_gender %in% c('Men', 'Women') & year == 2020) %>%
  group_by(industry, race_gender) %>%
  summarise(tot_employed = sum(employ_n)) %>%
  spread(race_gender, tot_employed) %>%
  mutate(Total = Men + Women,
         per_women = Women / Total) %>%
  filter(!is.na(per_women))

ggplot(women_2020, aes(fct_reorder(industry, per_women), per_women)) + 
  geom_col() +
  coord_flip() + 
  labs(x = '', y = '% of Women Employees') +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

### Employment by race
race_2020 <- employed %>% 
  filter(race_gender %in% c('White', 'Black or African American', 'Asian') & year == 2020) %>%
  group_by(industry, race_gender) %>%
  summarise(tot_employed = sum(employ_n)) %>%
  spread(race_gender, tot_employed) %>%
  mutate(Total = Asian + `Black or African American` + White,
         per_asian = Asian / Total,
         per_black = `Black or African American` / Total,
         per_white = White / Total) %>%
  mutate_if(is.numeric, round, 2) %>%
  filter(!is.na(per_asian))

race_2020 <- race_2020 %>% 
  select(industry, Asian = per_asian, Black = per_black) %>%
  mutate(per_black_ordering = Black) %>% # so graph can be ordered
  gather(metric, val, c(Asian, Black))

ggplot(race_2020, aes(fct_reorder(industry, per_black_ordering), val, fill = metric)) + 
  geom_bar(stat="identity", position = "dodge") +
  coord_flip() + 
  labs(x = '', y = '% of Employees', fill = 'Race') +
  guides() +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

#### Earnings ####
### Adults by Race & Sex
adult_earnings <- earn %>%
  filter(sex != 'Both Sexes' & race != 'All Races' & age == '16 years and over') %>%
  group_by(sex, race, age, year) %>%
  summarise(n_persons = sum(n_persons),
            median_yearly_earn = 52 * median(median_weekly_earn)) %>%
  ungroup() %>%
  mutate(demo = paste(race, sex))

cols <- c("Asian Men" = "#003399", 
          "Black or African American Men" = "#FF2B4F", 
          "White Men" = "#fcab27", 
          "Asian Women" = "#3686d3",
          "Black or African American Women" = "#cf082a",
          "White Women" = "#f58516")

ggplot(adult_earnings, aes(year, median_yearly_earn, group = demo, color = demo)) +
  geom_path() +
  scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = '', y = 'Median Yearly Earnings', color = '',
       title = 'Median Yearly Earnings by Race & Sex',
       subtitle = 'Individuals 16 years and over',
       caption = 'Source: BLS') +
  #facet_grid(~ race) +
  theme_bw()

### By Race, Sex & Age
age_earnings <- earn %>%
  filter(sex != 'Both Sexes' & race != 'All Races' & 
           !(age %in% c('16 years and over', '25 years and over'))) %>%
  group_by(sex, race, age, year) %>%
  summarise(n_persons = sum(n_persons),
            median_yearly_earn = 52 * median(median_weekly_earn)) %>%
  ungroup() %>%
  mutate(demo = paste(race, sex))

ggplot(age_earnings, aes(year, median_yearly_earn, group = demo, color = demo)) +
  geom_path() +
  scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = '', y = 'Median Yearly Earnings', color = '',
       title = 'Median Yearly Earnings by Race & Sex') +
  facet_wrap(~ age) +
  theme_bw()