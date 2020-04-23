library(tidyverse)
library(scales)
library(lubridate)
library(ggrepel)
library(ggwaffle)

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

gdpr_violations <- gdpr_violations %>%
  mutate(date = as.Date(date, '%m/%d/%y'))

## CUMULATIVE FINEs BY COUNTRY
cum_fines_df <- gdpr_violations %>%
  group_by(name) %>%
  summarize(
            tot_fines = n(),
            min_price = min(price),
            `Mean Fine` = mean(price),
            max_price = max(price),
            `Total Fines` = sum(price)
            ) %>%
  ungroup() %>%
  filter(`Mean Fine` > quantile(.$`Mean Fine`, 0.65))

cum_fines_df %>%
  gather(var_key, var_lab, c(`Mean Fine`, `Total Fines`)) %>%
  ggplot(aes(fct_reorder(name, var_lab), var_lab, fill = var_key)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = '', suffix = ' ???')) +
  scale_fill_manual(values = c("#003399", "#FF2B4F")) +
  labs(x= '', y = 'Amount', color = '', fill = '',
       title = 'GDPR Fines, Total Amount per Country', 
       subtitle = 'France, Italy, Austria and Germany amongst largest finers')

## NUMBER OF FINES PER COUNTRY
top_finers <- gdpr_violations %>%
  group_by(name) %>% 
  count() %>%
  ungroup() %>%
  top_n(3)

waffle_data <- gdpr_violations %>%
  mutate(country_name = case_when(
    name %in% top_finers$name ~ name,
    T ~ ' Other'
  )) %>%
  waffle_iron(aes_d(group = country_name))

waffle_fill <- c(' Other' = 'grey',
                # 'Bulgaria'  = '#93003a',
                 'Germany' = '#f4777f',
                 #'Hungary' = '#ffcc99',
                 'Romania' = '#ffcc99',
                 'Spain' = '#00429d')

ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle() +
  coord_equal() + 
  scale_fill_manual(values = waffle_fill) + 
  labs(x = '', y ='', fill = 'Country',
       title = 'GDPR Fines, Total Issued by Country',
       subtitle = 'Spain has issued the most fines by far (69), followed by Germany (27) and Romania (26)') +
  theme_waffle()

### ANOTHER WAFFLE
# First Aggregation to get % amount fined by country
per_fine_country <- gdpr_violations %>%
  group_by(name) %>%
  summarise(tot_fines_country = sum(price)) %>%
  mutate(tot_fines = sum(tot_fines_country),
         per_fines = tot_fines_country / tot_fines)

# Second to simplify for graph, grouping every country that is below 10% together
per_fine_country <- per_fine_country %>%
  mutate(name = case_when(
    per_fines < 0.1 ~ ' Other',
    T ~ name
  )) %>%
  group_by(name) %>%
  summarise(tot_fines_country = sum(tot_fines_country)) %>%
  mutate(tot_fines = sum(tot_fines_country),
         per_fines = round(100 * tot_fines_country / tot_fines, 0))

# Preparing the plot: ggwaffle's waffle_iron is based on count by country
# so we need to repeat the country so that the % of rows it has in the df maps
# to its % of blocks in the waffle plot
per_fine_country <- per_fine_country[rep(seq_len(nrow(per_fine_country)), max(per_fine_country$per_fines)), ] %>% #repeats df by highest amount of fines
  split(.$name) %>% #then we whittle it down so that countries appear up to the percentage of fines they are responsible for
  map(mutate, rownum = row_number()) %>%
  map(filter, per_fines >= rownum) %>%
  do.call('rbind', .) # sorry!

# Setting Color Mappings
waffle_fill <- c(' Other' = 'grey', #space so it appears at the top of legend
                 'France' = 'red',
                 'Germany' = '#f4777f',
                 'Italy' = '#93003a',
                 'Austria' = '#00429d')

# The final plot
per_fine_country %>%
  waffle_iron(aes_d(group = name)) %>%
  ggplot(aes(x, y, fill = group)) + 
  geom_waffle() +
  coord_equal() +
  scale_fill_manual(values = waffle_fill) + 
  labs(x = '', y ='', fill = 'Country',
       title = 'GDPR Fines, Total Amount Issued by Country',
       subtitle = 'France is responsible for 33% of the total value of GDPR fines, followed by Italy (26%) and Germany (16%)') +
  theme_waffle()

## OTHER PLOTS ###
## AVERAGE FINE BY COUNTRY (WORK IN PROGRESS)
top_fines <- gdpr_violations %>%
  group_by(type) %>% 
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 10)

#
waffle_fill <- c(' Other' = 'grey',
                 'France' = 'red',
                 'Germany' = '#f4777f',
                 'Italy' = '#93003a',
                 'Spain' = '#00429d')

avg_country_fines_by_type <- gdpr_violations %>%
  filter(type %in% top_fines$type) %>%
  mutate(country_label = 
           case_when(
             name %in% c('France', 'Italy', 'Germany', 'Spain') ~ name,
             T ~ ' Other'
           )) %>%
  group_by(name, country_label, type) %>%
  summarize(`# Fines` = n(),
            min_price = min(price),
            `Mean Fine` = mean(price),
            `Total Fines` = max(price)) %>%
  ungroup() 

key_countries <- avg_country_fines_by_type %>%
  split(.$type) %>%
  map(arrange, desc(`Mean Fine`)) %>%
  map(head, 2) %>%
  do.call('rbind', .) %>%
  mutate(plot_flag = 1)

avg_country_fines_by_type %>%
  left_join(key_countries) %>%
  mutate(type = case_when(
    type == "Non-compliance with lawful basis for data processing" ~ 'Data Processing',
    type == 'Failure to comply with data processing principles' ~  'Data Processing',
    type == 'Failure to implement sufficient measures to ensure information security' ~ 'Information Security',
    type == "Non-compliance with subjects' rights protection safeguards" ~ "Lack of subjects' rights protection safeguards",
    T ~ type
  )) %>%
  ggplot(aes(`# Fines`, `Mean Fine`, color = country_label, label = name)) +
  geom_point() + 
  geom_point(data = . %>% filter(plot_flag == 1), size = 2) + 
  geom_text_repel(data = . %>% filter(plot_flag == 1)) +
  scale_y_continuous(labels = dollar_format(prefix = '', suffix = ' ???')) +
  scale_color_manual(values = waffle_fill) +
  labs(title = 'GDPR Fines') +
  facet_wrap(~ type , scales = 'free_x')