### Week 7, TidyTuesday 2021
library(tidyverse)

student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')



### Wealth, Home Ownership, & Student Debt / Race over Time
# 3 fold story
# 1. high level of home ownership in the 80s due to redlining
# 2. higher levels of wealth for white families to start with
# advantages compound -> higher home ownership rates build up wealth
# white family wealth balloons
# 3. at the same time, increase in black & white student loan debt
# but no corresponding increase in wealth for black families - just more debt

cols <- c( 
          "Asian" = "#04662f",
          "Black" = "#FF2B4F", 
          "Non-White" = "#FF2B4F", 
          "White" = "#fcab27",
          "Hispanic" = '#003399'
          )

# Home Ownership
home_owner %>%
  ggplot(aes(year, home_owner_pct, color = race)) +
  geom_path() +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = '', y = '% Home Owners', color = 'Race',
       title = 'Differences in Home Ownership Rates Drive Racial Wealth Gap',
       subtitle = 'Biased Policies Led to Much Higher Home Ownership Rates for White Households') +
  theme_bw() 

# not enough data but can likely be traced to post-WWII
# would be nice to also have a graph with average home price or something like that to
# support claim

# Wealth
race_wealth %>%
  filter(year >= 1980 & race != 'Non-White') %>% # to match previous graph
  ggplot(aes(year, wealth_family, color = race)) +
  geom_path() +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = '', y = 'Household Wealth', color = 'Race',
       title = 'Racial Wealth Gap Has Widened Since The 80s',
       subtitle = 'Driven Upwards By Differences in Home Ownership Rates, Income Inequality
& Racial Discrimination') +
  facet_grid(~ type) +
  theme_bw()

# Student Debt
student_debt %>%
  ggplot(aes(year, loan_debt, color = race)) +
  geom_path() +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = '', y = 'Household Retirement Funds', color = 'Race',
       title = 'Average Student Loan Debt Tripled Between 2000 and 2015',
       subtitle = 'For Both Black & White Households, Without Reducing Wealth Gap') +
  theme_bw()

# Student Debt Burden
wealth_loans <- race_wealth %>%
  filter(type == 'Average') %>%
  select(-type) %>%
  inner_join(student_debt, by = c('year', 'race')) %>%
  mutate(loan_pct_wealth = loan_debt / wealth_family)

wealth_loans %>%
  ggplot(aes(year, loan_pct_wealth, color = race)) +
  geom_path() +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = '', y = 'Loan / Wealth', color = 'Race',
       title = 'Student Loan Debt Burden Much Higher for Non-White Households'
      ) +
  theme_bw()

### Income Inequality & Race
# for consistency, focusing on same categories as before
income_aggregate %>%
  filter(race %in% c('Black Alone', 'Hispanic', 'White Alone') &
           income_quintile != 'Top 5%') %>%
  mutate(
    race = case_when(
      race == 'Asian Alone' ~ 'Asian',
      race == 'Black Alone' ~ 'Black',
      race == 'White Alone' ~ 'White',
      T ~ race
    ),
    income_quintile = factor(income_quintile, levels = c("Lowest", "Second", "Third", "Fourth", "Highest"))
  ) %>%
  ggplot(aes(year, income_share / 100, color = race)) + 
  geom_path() + 
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = '', y = 'Share of Income', color = 'Race',
       title = 'Similar Income Growth Trends Within Racial Groups',
       subtitle = 'Faster Income Growth for the Top 20% Leads to Concentration of Wealth') +
  facet_grid(~ income_quintile) +
  theme_bw()

income_mean %>%
  filter(dollar_type == '2019 Dollars' &
           race %in% c('Black Alone', 'Hispanic', 'White Alone') &
           income_quintile != 'Top 5%') %>%
  mutate(
    race = case_when(
      race == 'Asian Alone' ~ 'Asian',
      race == 'Black Alone' ~ 'Black',
      race == 'White Alone' ~ 'White',
      T ~ race
    ),
    income_quintile = factor(income_quintile, levels = c("Lowest", "Second", "Middle", "Fourth", "Highest"))
  ) %>%
  ggplot(aes(year, income_dollars, color = race))+
  geom_line() +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = '', y = 'Income', color = 'Race',
       title = 'Income Differences Persist & Fuel Unequal Distribution of Wealth') +
  facet_grid(~ income_quintile) +
  theme_bw()
