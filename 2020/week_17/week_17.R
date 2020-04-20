library(tidyverse)
library(scales)
library(lubridate)

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

gdpr_violations <- gdpr_violations %>%
  mutate(date = as.Date(date, '%m/%d/%y'))


## MOST COMMON types
common_types_violation <- gdpr_violations %>%
  group_by(type) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(2)

gdpr_violations %>%
  mutate(date = as.Date(date, '%m/%d/%y')) %>%
  mutate(type_label = case_when(
    type == common_types_violation$type[1] ~ 'Poor data processing',
    type == common_types_violation$type[2] ~ 'Lack of information security',
    T ~ ' Other'
  )) %>%
  filter(date >= '2020-01-01') %>%
  ggplot(aes(date, price, color = type_label)) +
  geom_point() +
  geom_text(data = . %>% filter(price > quantile(price, 0.97)),
                                aes(label = name), nudge_y =  1500000) +
  scale_y_continuous(labels = dollar_format(prefix = '', suffix = ' ???')) +
  scale_color_manual(values = c("darkgrey", "#003399", "#FF2B4F")) +
  labs(x= '', y = 'Fine Amount', color = 'Reason', 
       title = 'GDPR Fines', subtitle = 'Fines Over Time')+ 
  theme(legend.position="bottom")

## CUMULATIVE FINEs BY COUNTRY
gdpr_violations %>%
  group_by(name) %>%
  summarize(
            tot_fines = n(),
            min_price = min(price),
            `Mean Fine` = mean(price),
            max_price = max(price),
            `Total Fines` = sum(price)
            ) %>%
  ungroup() %>%
  filter(`Mean Fine` > quantile(.$`Mean Fine`, 0.65)) %>%
  gather(var_key, var_lab, c(`Mean Fine`, `Total Fines`)) %>%
  ggplot(aes(fct_reorder(name, var_lab), var_lab, fill = var_key)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = '', suffix = ' ???')) +
  scale_fill_manual(values = c("#003399", "#FF2B4F")) +
  labs(x= '', y = 'Amount', color = '', fill = '',
       title = 'GDPR Fines, Amount per Country', 
       subtitle = 'France, Italy, Austria and Germany amongst largest finers')

## AVERAGE PRICE PER TYPE VIOLATION
gdpr_violations %>%
  #filter(type %in% common_types_violation$type) %>%
  group_by(type) %>%
  summarize(tot_fines = n(),
            min_price = min(price),
            mean_price = mean(price),
            max_price = max(price)) %>%
  ggplot(aes(tot_fines, mean_price)) + geom_point()
  