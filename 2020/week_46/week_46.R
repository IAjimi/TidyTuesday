library(tidyverse)

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

# Plot 1: Mobile Subscriptions per Person over Time
ggplot(mobile, aes(year, mobile_subs / 100, group = entity)) +
  geom_path(color = 'grey', alpha = 0.8) +
  geom_smooth(aes(group = continent), se = FALSE, color = 'red') +
  facet_grid(~ continent) + 
  labs(x = '', y = 'Avg Mobile Subscription per Person')


# Plot 2: Relationship between gpd_per_cap and mobile_subs over time
## Check years with: mobile %>% filter(!is.na(gdp_per_cap) & !is.na(mobile_subs) & !is.na(total_pop)) %>% distinct(year)

## Finding countries to highlight
top_5_mobile <- mobile %>%
  group_by(entity) %>%
  summarise(max_mobile_subs = max(mobile_subs)) %>%
  arrange(desc(max_mobile_subs)) %>%
  ungroup()%>%
  top_n(5)

## Plot
mobile %>%
  filter(year %in% c(1990, 1995, 2000, 2005, 2010)) %>%
  ggplot(aes(gdp_per_cap, mobile_subs / 100, size = total_pop)) +
  geom_point(alpha = 0.6) +
  geom_point(data = . %>% filter(entity %in% c('Hong Kong', 'China', 'United States')), 
             aes(color = entity), alpha = 0.6) +
  facet_grid(~ year) + 
  labs(x = 'GDP per Capita', y = 'Avg Mobile Subscription per Person', size = 'Population')

# Plot 3:
phones <- inner_join(mobile, landline, by = c('entity', 'continent', 'code', 'year'))
phones <- phones %>%
  filter(!is.na(mobile_subs) & !is.na(landline_subs)) %>%
  mutate(tot_pop = (total_pop.x + total_pop.y) / 2,
         gdp_per_cap = (gdp_per_cap.x + gdp_per_cap.y) / 2,
         mobile_to_landline = mobile_subs / landline_subs)

phones %>% 
  filter(mobile_to_landline <= 20) %>%
  ggplot(aes(year, mobile_to_landline, color = entity)) + 
  geom_path(color = 'grey', alpha = 0.8) +
  geom_smooth(aes(group = continent), se = FALSE, color = 'red') +
  facet_grid(~ continent) + 
  labs(x = '', y = 'Avg Mobile Subscription per Person')
# only countries w full data