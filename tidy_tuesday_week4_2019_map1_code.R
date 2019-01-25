library(maps)
library(tidyverse)

#Getting Map Data
us_counties <- map_data("county")
us_counties$polyname <- paste0(us_counties$region, ",", us_counties$subregion) 
#puts names in format compatible w county.fips df
us_counties <- left_join(us_counties, county.fips) 

#Getting Summary Data at State Level
incarceration_trends <- read_csv("C:/Users/ia767/Downloads/incarceration_trends.csv")

prison_county_sum <- incarceration_trends %>% filter(year >= 2010) %>% 
  group_by(fips) %>% 
  summarise(per_bla_pop = mean(black_pop_15to64/total_pop, na.rm = TRUE),
            per_bla_prison = mean(black_jail_pop/total_jail_pop, na.rm = TRUE),
            gap = per_bla_prison - per_bla_pop) %>%
  filter(per_bla_prison <= 1) #filters out counties w inaccurate data

#Creating Map
prison_county <- left_join(us_counties, prison_county_sum, by = "fips")

ggplot(prison_county,
       aes(x = long, y = lat, group = group, fill = gap)) + 
  geom_polygon(color = NA, size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + #curves map
  scale_fill_gradient2(high = "red",
                       mid = scales::muted("purple"),
                       low = "blue") +
  guides(fill = FALSE) + #gets rid of color guide
  theme(axis.title.y = element_blank(), #gets rid of x and y axis labels
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Southern & Eastern U.S. Imprison Disproportionate Number of African-Americans", 
       subtitle = "Difference between the % of Af-Am in Prison and in Population")
