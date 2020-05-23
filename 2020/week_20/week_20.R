
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

volcano %>% head()
eruptions %>% head()

## Sulfur levels and eruption events
sulfur <- sulfur %>%
  mutate(year = as.integer(year)) %>%
  group_by(year) %>%
  summarise_if(is.numeric, mean)
  
eruptions %>%
  filter(eruption_category == 'Confirmed Eruption') %>%
  rename(year = start_year) %>%
  group_by(year) %>%
  count() %>% 
  right_join(sulfur) %>%
  #mutate(n = replace_na(n, 0)) %>%
  gather(var_key, var_val, -year) %>%
  ggplot(aes(year, var_val, color = var_key)) +
  geom_point() +
  facet_grid(~ var_key)

## Eruption frequency and nearby population
eruptions %>%
  filter(eruption_category == 'Confirmed Eruption' & start_year >= 1500) %>%
  left_join(volcano) %>%
  group_by(volcano_name) %>%
  summarise(number_eruptions = n(),
            population_within_5_km = mean(population_within_5_km, na.rm = T),
            population_within_30_km = mean(population_within_30_km, na.rm = T)) %>%
  ggplot(aes(population_within_30_km, number_eruptions)) +
  geom_jitter(alpha = 0.5) +
  labs(x = 'Population within 30 km', y = 'Number of Eruptions Since 1500') +
  theme_bw()


## Most active volcanos
most_active_volcanos <- eruptions %>%
  filter(eruption_category == 'Confirmed Eruption' & start_year >= 1000) %>%
  mutate(start_date = paste(start_year, start_month, start_day, sep = '/'),
         start_date = as.Date(start_date),
         end_date = paste(end_year, end_month, end_day, sep = '/'),
         end_date = as.Date(end_date),
         eruption_duration = end_date - start_date) %>%
  group_by(volcano_number, volcano_name) %>%
  summarize(eruption_duration = mean(eruption_duration, na.rm = T),
            n_eruptions = n()) %>%
  ungroup() %>%
  top_n(8, n_eruptions)


eruptions %>%
  filter(eruption_category == 'Confirmed Eruption' & start_year >= 1900 &
           volcano_name %in% most_active_volcanos$volcano_name) %>%
  mutate(start_date = paste(start_year, start_month, start_day, sep = '/'),
         start_date = as.Date(start_date),
         end_date = paste(end_year, end_month, end_day, sep = '/'),
         end_date = as.Date(end_date),
         eruption_duration = end_date - start_date
         ) %>%
  group_by(volcano_number, volcano_name) %>%
  summarize(eruption_duration_median = median(eruption_duration, na.rm = T),
            eruption_duration_q2 = quantile(eruption_duration, 0.25, na.rm = T),
            eruption_duration_q3 = quantile(eruption_duration, 0.75, na.rm = T)
            ) %>%
  ggplot(aes(fct_reorder(volcano_name, eruption_duration_median), eruption_duration_median)) +
  geom_pointrange(aes(
                      ymin = eruption_duration_q2, 
                      ymax = eruption_duration_q3
                      ),
                  color =  '#ff661a'
                  ) +
  coord_flip() +
  labs(y = 'Eruption Duration (Days)', x = '',
       title = "The world's most active volcanos are also active for long periods of time") +
  theme_bw()
