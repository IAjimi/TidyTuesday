# TIDYTUESDAY 2021 WEEK 18
#### SETUP ####
library(tidyverse)

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')
gender_names <- readr::read_delim('https://github.com/MatthiasWinkelmann/firstname-database/raw/master/firstnames.csv', delim = ';') 

departures <- departures %>% 
  select(coname, fyear, departure_code, exec_fullname, tenure_no_ceodb, max_tenure_ceodb,
         fyear_gone) %>%
  filter(!is.na(departure_code)) %>%
  mutate(departure_reason = case_when(
    departure_code == 1 ~ 'Death',
    departure_code == 2 ~ 'Illness',
    departure_code == 3 ~ 'Dismissed for job performance',
    departure_code == 4 ~ 'Dismissed for legal reasons',
    departure_code == 5 ~ 'Retirement',
    departure_code == 6 ~ 'New opportunity',
    departure_code == 7 ~ 'Other',
    departure_code == 8 ~ 'Missing',
    departure_code == 9 ~ 'Missing'
  ))


gender_names <- gender_names %>% 
  select(name, gender) %>%
  mutate(gender = case_when(
    gender == '?' ~ NA_character_,
    gender %in% c('?F', '1F') == TRUE ~ 'F',
    gender %in% c('?M', '1M') == TRUE ~ 'M',
    TRUE ~ gender
  ))

departures <- departures %>%
  mutate(first_name = exec_fullname %>% str_split(' ') %>% map_chr(1)) %>%
  inner_join(gender_names, by = c('first_name' = 'name'))

departures %>%
  summary()

# Execs in multiple companies
departures %>%
  select(exec_fullname, coname) %>%
  distinct() %>%
  group_by(exec_fullname) %>%
  count() %>%
  arrange(desc(n))

# Breaking Departure Reasons down by Gender
gendered_departures <- departures %>% 
  filter(!departure_reason %in% c('Missing', 'Other')) %>%
  group_by(gender, departure_reason) %>%
  count() %>%
  ungroup()

gender_departure <- departures %>% 
  group_by(gender) %>%
  count() %>%
  ungroup() %>%
  rename(total = n)

gendered_departures <- gendered_departures %>% 
  inner_join(gender_departure, by = 'gender') %>%
  mutate(per = 100 * n /total) 

# All Departure Reasons
gendered_departures %>%
  ggplot(aes(fill=departure_reason, y=per, x='All CEOs')) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  labs(x = '', y = '%', fill = 'Reason',
       title = 'CEO Departure Reasons',
       subtitle = 'The majority of CEOs leave for retirement')

# Departure Reasons by Gender
gendered_departures %>%
  filter(gender %in% c('F', 'M')) %>%
  ggplot(aes(fill=departure_reason, y=per, x=gender)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  labs(x = 'Gender', y = '%', fill = 'Reason',
       title = 'CEO Departure Reasons by Gender',
       subtitle = 'Women more likely to be dismissed')
