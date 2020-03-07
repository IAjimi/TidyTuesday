## Loading Libraries
library(tidyverse)
library(gganimate)

## Reading Data
confirmed_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recoveries <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

## Helper Function: Format dataset
format_df <- function(df){
  df <- df %>% 
    gather(date_key, date_val, -Lat, -Long, - `Country/Region`, -`Province/State`) %>%
    mutate(date_key = as.Date(date_key, "%m/%d/%y"))
  
  return(df)
}

## Clean DFs
confirmed_cases <- confirmed_cases %>% format_df()
deaths <- deaths %>% format_df()
recoveries <- recoveries %>% format_df()

### World Map ####
#partly from https://stackoverflow.com/questions/30706124/plotting-the-world-map-in-r
WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

## STATIC: Most Recent Date
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  geom_point(data = filter(confirmed_cases, date_key == "2020-03-04"), 
             aes(Long, Lat, size = date_val, color = "#db4551"), alpha = 0.6) +
  #scale_color_manual(values = c("#db4551", '#455edb')) +
  guides(size = FALSE, color = FALSE) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(title="COVID-19", subtitle = "Confirmed Cases, 2020-03-04",
       x="", y="", color = "", caption = "Source: John Hopkins University") +
  theme_bw()

## GIF: Over Time
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  geom_point(data = confirmed_cases, 
             aes(Long, Lat, size = date_val, color = "#db4551"), alpha = 0.6) +
  #scale_color_manual(values = c("#db4551", '#455edb')) +
  transition_states(
    date_key,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  guides(size = FALSE, color = FALSE) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(title="COVID-19", subtitle = "Confirmed Cases",
       x="", y="", color = "", caption = "Source: Johns Hopkins University") +
  theme_bw()


### DISEASE SPREAD SINCE FIRST CONFIRMED CASE ####
confirmed_cases_by_country <- confirmed_cases %>% 
  group_by(`Country/Region`, date_key) %>% 
  summarise(date_val = sum(date_val)) %>%
  ungroup()

most_cases_country <- confirmed_cases_by_country %>% 
  filter(date_key == max(.$date_key) & `Country/Region` != "Others") %>% 
  arrange(desc(date_val)) %>% 
  top_n(5, date_val)

confirmed_cases_spread <- confirmed_cases_by_country %>% 
  split(.$`Country/Region`) %>%
  map(filter, date_val > 0) %>%
  map(function(x) mutate(x, x_axis = c(1:nrow(x))))

confirmed_cases_spread <- do.call("rbind.data.frame", confirmed_cases_spread)

color_scheme <- c("Mainland China" = "#003399",
                   "Iran" = "#FF2B4F",
                  "Italy" = "#fcab27",
                  "South Korea" = "#3686d3",
                  "Germany" = "darkgrey")

confirmed_cases_spread %>%
  filter(`Country/Region` %in% most_cases_country$`Country/Region`) %>%
  ggplot(aes(x_axis, date_val, color = `Country/Region`)) +
  geom_line(size = 1) +
  scale_color_manual(values = color_scheme) + 
  labs(x = "Days since 1st Recorded Case", y = "# Confirmed Cases", color = "Country",
       title = "Covid-19", subtitle = "Spread Since 1st Confirmed Case")
