library(tidyverse)

incarceration_trends <- read_csv("C:/Users/ia767/Downloads/incarceration_trends.csv")

### REGIONAL DATA ###
reg_prison <- incarceration_trends %>% filter(year >= 1990) %>% group_by(year, region) %>% 
  summarise(per_pop_prison = mean(total_jail_pop/total_pop, na.rm = TRUE),
            per_fem_prison = mean(female_jail_pop/total_jail_pop, na.rm = TRUE),
            admin_per_prisoner = mean(total_jail_adm/total_jail_pop, na.rm = TRUE))

ggplot(reg_prison, aes(year, per_pop_prison)) + geom_line(aes(color = region)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sharp Increase in Proportion of Population In Prison",
       subtitle = "Trend Driven by the South and West",
       x = "", y = "Percentage of Population in Prison", color = "Region")

ggplot(reg_prison, aes(year, per_fem_prison)) + geom_line(aes(color = region)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Steady Increases in Proportion of Incarcerated Women", 
       x = "", y = "Percentage of Female Prisoners", color = "Region")

### STATE DATA ####
sta_prison <- incarceration_trends %>% filter(year >= 1990) %>% 
  group_by(year, region, state) %>% 
  summarise(per_pop_prison = mean(total_jail_pop/total_pop, na.rm = TRUE),
            per_fem_prison = mean(female_jail_pop/total_jail_pop, na.rm = TRUE),
            admin_per_prisoner = mean(total_jail_adm/total_jail_pop, na.rm = TRUE))

ggplot(sta_prison, aes(year, per_pop_prison)) + geom_line(aes(group = state), color = "grey") +
  geom_smooth(se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sharp Increase in Proportion of Population In Prison",
       subtitle = "Trend Driven by the South and West",
       x = "", y = "Percentage of Population in Prison", color = "Region") +
  facet_grid(~ region)


### LOOKING AT CHANGES IN SUBGROUP REPRESENTATION IN PRISON ####
prison <- incarceration_trends %>% filter(year >= 1990) %>% group_by(year, urbanicity, region) %>% 
  summarise(per_bla_prison = mean(black_jail_pop/total_jail_pop, na.rm = TRUE),
            per_fem_prison = mean(female_jail_pop/total_jail_pop, na.rm = TRUE),
            admin_per_prisoner = mean(total_jail_adm/total_jail_pop, na.rm = TRUE))

ggplot(prison, aes(year, per_fem_prison)) + geom_line(aes(color = urbanicity)) +
  facet_wrap(~ region) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Steady Increases in Proportion of Incarcerated Women", 
       x = "", y = "Percentage of Female Prisoners", color = "Urbanicity")

### STATE-MAP (plot 5) ####
library(maps)

us_states <- ggplot2::map_data("state") #getting long lat data
states <- data.frame(state = state.abb, state_name = tolower(state.name)) 
#creating df to map state abbs to state names

#getting summary data at state level
prison_state_sum <- incarceration_trends %>% filter(year >= 2010) %>% group_by(state) %>% 
  summarise(per_pop_prison = mean(total_jail_pop/total_pop, na.rm = TRUE))

#merging data sets
prison_state <- left_join(states, prison_state_sum, by = "state")
prison_state <- left_join(prison_state, us_states, by = "region")

ggplot(prison_state,
       aes(x = long, y = lat, group = group, fill = per_pop_prison)) + 
  geom_polygon(color = NA, size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + #curves map
  scale_fill_gradient(low = "white", high = "#CB454A") +
  guides(fill = FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Southern and Western States Leading Nation in Imprisonment Rates",
       subtitle = "Percentage of the Population in Prison, 2010-2015")