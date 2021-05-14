### WEEK 9, 2021 TIDYTUESDAY
library(tidyverse)

employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

#### Cleaning DF ####
employed <- employed %>%
  filter(
    year == 2020 & !is.na(employ_n)
  ) %>%
  mutate(
  industry = case_when(
    industry == 'Other services, except private households' ~ 'Other services',
    industry %in% c('Retail trade', 'Wholesale trade') ~ 'Wholesale and retail trade',
    T ~ industry
  ),
  race_gender = if_else(race_gender == 'Black or African American', 'Black', race_gender)
)

### Focusing on Sex By Industry ####
sex_by_industry <- employed %>% 
  filter(race_gender %in% c('Men', 'Women')) %>%
  group_by(industry, race_gender) %>%
  summarise(tot_employed = sum(employ_n, na.rm=T)) %>%
  spread(race_gender, tot_employed) %>%
  mutate(
         Men = Men / sum(.$Men, na.rm=T),
         Women = Women / sum(.$Women, na.rm=T),
         Diff = abs(Men - Women) / Women
         ) # map_if(is.numeric, ~./sum(., na.rm=T))

# Get important industries
imp_industries <- sex_by_industry %>% 
  arrange(desc(Diff)) %>%
  head(8) %>%
  pull(industry)

# Group by important industries, Gather Men, Women Columns for Graph
sex_by_industry <- sex_by_industry %>%
  mutate(
    industry = case_when(
      industry %in% imp_industries ~ industry,
      T ~ 'Other'
    )
  ) %>%
  group_by(industry) %>%
  summarise_if(is.numeric, sum) %>%
  gather(sex_key, sex_val, c(Men, Women))

# 1st plot - shows % Employed by Industry for all industries
sex_by_industry %>%
  ggplot(aes(x=sex_key, y=sex_val, fill=industry)) + 
    geom_bar(position="fill", stat="identity", color = 'white') +
    viridis::scale_fill_viridis(discrete = T) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = '', y = '% in Industry', fill = 'Industry',
         title = 'Employment by Sex',
         subtitle = 'Large differences in employment by sex remain') +
    theme_bw()

# 2nd plot - shows % Employed by Industry side-by side for select industries
sex_by_industry %>%
  filter(industry != 'Other') %>%
  ggplot(aes(fct_reorder(industry, sex_val), sex_val, fill = sex_key)) + 
    geom_bar(stat="identity", position = "dodge") +
    coord_flip() + 
    scale_fill_manual(values = c("#003399", "#FF2B4F")) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = '', y = '% in Industry', fill = 'Sex',
         title = 'Employment by Sex',
         subtitle = 'Large differences in employment by sex remain') +
    theme_bw()

# gendered in pretty much the way you would expect
# 30% of women work in education and health services
# women are under-represented in manufacturing, construction, durable goods,
# transportation and utilities, etc

#### Focusing on Race By Industry ####
race_by_industry <- employed %>% 
  filter(
    race_gender %in% c('White', 'Black', 'Asian')
  ) %>%
  group_by(industry, race_gender) %>%
  summarise(tot_employed = sum(employ_n, na.rm=T)) %>%
  spread(race_gender, tot_employed) %>%
  mutate(
    Asian = Asian / sum(.$Asian, na.rm=T),
    Black = Black / sum(.$Black, na.rm=T),
    White = White / sum(.$White, na.rm=T),
    Diff = abs(Black - White) / White
  ) # map_if(is.numeric, ~./sum(., na.rm=T))


# 1st plot - shows % Employed by Industry side-by side for select industries
cols <- c("Asian" = "#003399", 
          "Black" = "#FF2B4F", 
          "White" = "#fcab27")

imp_industries <- c('Education and health services',
                    'Professional and business services',
                    'Financial activities',
                    'Public administration',
                    'Construction',
                    'Information',
                    'Agriculture and related')

race_by_industry %>%
  gather(key, val, -c(industry, Diff)) %>%
  filter(industry %in% imp_industries) %>%
  ggplot(aes(fct_reorder(industry, val), val, fill = key)) + 
  geom_bar(stat="identity", position = "dodge") +
  coord_flip() + 
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = '', y = '% in Industry', fill = 'Race',
       title = 'Employment by Race',
       subtitle = 'Large differences in employment by race remain') +
  theme_bw()