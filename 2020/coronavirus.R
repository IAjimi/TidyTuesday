## SETUP ####
## Loading Libraries
library(tidyverse)
library(gganimate)

## Reading Data
confirmed_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recoveries <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

## HELPER FUNCTIONS ####
## Helper Function: Format dataset
format_df <- function(df){
  df <- df %>% 
    gather(date_key, date_val, -Lat, -Long, - `Country/Region`, -`Province/State`) %>%
    mutate(date_key = as.Date(date_key, "%m/%d/%y"))
  
  return(df)
}

## Helper Function: Sum by Country
country_current_total <- function(df){
  df <- df %>%
    group_by(`Country/Region`, date_key) %>% 
    summarise(date_val = sum(date_val)) %>%
    ungroup()
    
  return(df)
}

## Helper Function: Latest Total
latest_total <- function(df){
  df <- df %>%
    country_current_total() %>%
    filter(date_key == max(.$date_key)) %>%
    select(-date_key) %>%
    rename(Total = date_val) %>%
    arrange(desc(Total))
  
  return(df)
}


## DATA PREP ####
## Clean DFs
confirmed_cases <- confirmed_cases %>% format_df()
deaths <- deaths %>% format_df()
recoveries <- recoveries %>% format_df()
### World Map ####
#partly from https://stackoverflow.com/questions/30706124/plotting-the-world-map-in-r
WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify 

# Adding Column Flag for Countries w Confirmed Cases
countries_w_confirmed_cases <- confirmed_cases %>% 
  filter(date_val > 0) %>% 
  distinct(`Country/Region`) %>%
  mutate(`Country/Region` = case_when(
    `Country/Region` == "US" ~ "USA",
    `Country/Region` == "Mainland China" ~ "China",
    T ~ `Country/Region`
  ))

WorldData <- WorldData %>% 
  mutate(infected = region %in% countries_w_confirmed_cases$`Country/Region`)

## GRAPHS ####
## STATIC: Most Recent Date
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region, fill = infected),
           colour = "#c8c8c8", size=0.5) + 
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  geom_point(data = confirmed_cases %>% filter(date_key == max(.$date_key)), 
             aes(Long, Lat, size = date_val, color = "#db4551"), 
             alpha = 0.8) +
  scale_fill_manual(values = c("#f7f7f7", "#fff4c3")) +
  guides(size = FALSE, color = FALSE, fill = FALSE) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(title="COVID-19", subtitle = paste("Confirmed Cases", max(confirmed_cases$date_key), sep = ", "),
       x="", y="", color = "", caption = "Source: John Hopkins University") +
  theme_bw()

## GIF: Over Time
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region, fill = infected),
           colour = "#c8c8c8", size=0.5) + 
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  geom_point(data = confirmed_cases %>% filter(date_key == max(.$date_key)), 
             aes(Long, Lat, size = date_val, color = "#db4551"), 
             alpha = 0.8) +
  scale_fill_manual(values = c("#f7f7f7", "#fff4c3")) +
  guides(size = FALSE, color = FALSE, fill = FALSE) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(title="COVID-19", subtitle = "Confirmed Cases Over Time",
       x="", y="", color = "", caption = "Source: John Hopkins University") +
  theme_bw() +
  transition_states(
    date_key,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')


### DISEASE SPREAD SINCE FIRST CONFIRMED CASE ####
confirmed_cases_by_country <- confirmed_cases %>% country_current_total()

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
                  "France" = "darkgrey")

confirmed_cases_spread %>%
  filter(`Country/Region` %in% most_cases_country$`Country/Region`) %>%
  ggplot(aes(x_axis, date_val, color = `Country/Region`)) +
  geom_line(size = 1) +
  scale_color_manual(values = color_scheme) + 
  labs(x = "Days since 1st Recorded Case", y = "# Confirmed Cases", color = "Country",
       title = "Covid-19", subtitle = "Spread Since 1st Confirmed Case")

### TABLES ####
total_cases <- confirmed_cases %>% latest_total() %>% rename(`# Confirmed Cases` = `Total`)
total_deaths <- deaths %>% latest_total() %>% rename(`# Deaths` = `Total`)
total_recoveries <- recoveries %>% latest_total()%>% rename(`# Recoveries` = `Total`)

# Display Table
total_cases %>% inner_join(total_deaths) %>% inner_join(total_recoveries)
