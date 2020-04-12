library(tidyverse)
library(scales)
library(plotly)

## IMPORT DATA 
usa_county_corona <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

## PREPARE DATA
confirmed_cases_spread <- usa_county_corona %>% 
  filter(cases > 100) %>%
  split(.$county) %>%
  purrr::map(function(x) mutate(x, x_axis = c(1:nrow(x)))) %>%
  purrr::map(mutate,
             lag_cases = lag(cases),
             lag_deaths = lag(deaths),
             cases_growth = (cases - lag_cases) / lag_cases,
             death_growth = (deaths - lag_deaths) / lag_deaths) %>%
  do.call("rbind.data.frame", .)

## PLOTS
noteworthy_county <- c("New York City", "San Francisco")

color_scheme <- c("New York City" = "#003399", "San Francisco" = "#FF2B4F", 
                  "Other" = "darkgrey")

p <- confirmed_cases_spread %>%
  mutate(county_label = case_when(
    county %in% noteworthy_county ~ county,
    T ~ "Other"
  )) %>%
  ggplot(aes(x_axis, cases, group = fips,  color = county_label)) +
  geom_line(data = . %>% filter(!county %in% noteworthy_county), alpha = 0.25) + #plots all other countries
  geom_line(data = . %>% filter(county %in% noteworthy_county), size = 1) +
  geom_point(data = . %>% filter(date == max(date) & county_label != "Other")) +
  scale_x_continuous(breaks = seq(0, 80, 7), limits = c(0, 31)) +
  scale_y_continuous(breaks = c(1, 100, 1000, 10000, 100000), 
                     labels = c(1, 100, 1000, 10000, "100000"), #avoid R's exponential notation
                     trans = "log"
  ) +
  scale_color_manual(values = color_scheme) + 
  labs(x = "Days since 100th Recorded Case", y = "# Confirmed Cases", color = "County")

p %>% ggplotly()

## GROWTH IN CONFIRMED CASES
confirmed_cases_spread %>%
  mutate(county_label = case_when(
    county %in% noteworthy_county ~ county,
    T ~ "Other"
  )) %>%
  ggplot(aes(x_axis, cases_growth, group = fips,  color = county_label)) +
  geom_line(data = . %>% filter(!county %in% noteworthy_county), alpha = 0.25) + #plots all other countries
  geom_point(data = . %>% filter(date == max(date) & county_label != "Other")) +
  geom_line(data = . %>% filter(county %in% noteworthy_county), size = 1) +
  scale_x_continuous(breaks = seq(0, 80, 7), limits = c(0, 31)) +
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25), labels = percent) + 
  scale_color_manual(values = color_scheme) + 
  labs(x = "Days since 100th Recorded Case", y = "Growth in Confirmed Cases", color = "County")

## GROWTH IN DEATHS
confirmed_cases_spread %>%
  mutate(county_label = case_when(
    county %in% noteworthy_county ~ county,
    T ~ "Other"
  )) %>%
  ggplot(aes(x_axis, death_growth, group = fips,  color = county_label)) +
  geom_line(data = . %>% filter(!county %in% noteworthy_county), alpha = 0.15) + #plots all other countries
  geom_point(data = . %>% filter(date == "2020-03-22" & county_label != "Other"), 
             shape = 17, size = 2.5) + #date of shelter in place order
  geom_point(data = . %>% filter(date == max(date) & county_label != "Other")) +
  geom_line(data = . %>% filter(county %in% noteworthy_county), size = 1) +
  scale_x_continuous(breaks = seq(0, 80, 7), limits = c(0, 31)) +
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25), labels = percent) + 
  scale_color_manual(values = color_scheme) + 
  labs(x = "Days since 100th Recorded Case", y = "Growth in Deaths", color = "County")


## GROWTH IN CONFIRMED CASES: COUNTRY + REGION ###
confirmed_cases <- usa_county_corona %>%
  rename(`Province/State` = state,
         `Country/Region` = county,
         date_key = date,
         date_val = cases) %>%
  mutate(Lat = NA, Long = NA) %>%
  select(-fips, -deaths) %>%
  rbind(confirmed_cases)

## PREPARE DATA
confirmed_cases_spread <- confirmed_cases %>% 
  filter(date_val > 100  & !(`Province/State` %in% c("Guadeloupe", "Reunion"))) %>%
  split(.$`Country/Region`) %>%
  purrr::map(function(x) mutate(x, x_axis = c(1:nrow(x)))) %>%
  purrr::map(mutate,
             lag_cases = lag(date_val),
             cases_growth = (date_val - lag_cases) / lag_cases) %>%
  do.call("rbind.data.frame", .)

noteworthy_county <- c("Korea, South", "Poland", "Belgium", "France", "US", "Japan", 
                       "New York City", "Brazil", "United Kingdom")

confirmed_cases_spread %>%
  filter(`Country/Region` %in% noteworthy_county) %>%
  ggplot(aes(x_axis, cases_growth, group = `Country/Region`)) +
  geom_line(data = . %>% filter(`Country/Region` != "New York City"), color = 'grey', alpha = 0.85) +
  geom_point(data = . %>% filter(date_key == max(date_key) & `Country/Region` != "New York City"), 
             color = 'grey', alpha = 0.85) +
  geom_line(data = . %>% filter(`Country/Region` == "New York City"), size = 1,
            color = '#003399') +
  geom_point(data = . %>% filter(date_key == max(date_key) & `Country/Region` == "New York City")) +
  scale_x_continuous(breaks = seq(0, 80, 7)) +
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25), labels = percent) + 
  labs(x = "Days since 100th Recorded Case", y = "Growth in Confirmed Cases")


############

library(socviz)
library(plotly)

## IMPORT DATA 
usa_county_corona <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
county_map <- socviz::county_map %>% rename(fips = id)

## PUT TOGETHER
county_graph <- county_map %>% left_join(usa_county_corona, by = "fips")

p <- county_graph %>% 
  filter(date == max(date, na.rm = T)) %>%
  right_join(county_map, 
             by = c("long", "lat", "order", "hole", "piece", "group", "fips")) %>% #re-add counties with missing info
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% #and set cases and deaths to 0
  #filter(state != 'Alaska') %>%
  mutate(Location = paste(county, ", ", state, '\n', 
                          'Confirmed cases:', cases, sep = '')) %>%
  ggplot(aes(x = long, y = lat, fill = cases, group = fips, label = Location))  + 
  geom_polygon(color = NA) + #no border colors 
  coord_equal() +
  scale_fill_continuous(
    low = "white", #white or #f7f1be
    high = "#f2161d",
    na.value = "white",
    trans = 'log',
    labels = c('', '20', '400', '8000'), #manual
  ) +
  guides(fill = guide_legend(nrow = 1)) + #breaks down legend
  labs(x= '', y ='', fill = "Total Confirmed Cases",
       title = paste('COVID-19 Spread: Confirmed Cases,', max(county_graph$date, na.rm = T))
  ) +
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "bottom")

p %>% ggplotly(tooltip = 'Location')

######################### UPDATE
library(socviz)
library(plotly)
library(tidyverse)

library(xml2)
library(httr)
library(rvest)

## IMPORT DATA 
usa_county_corona <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
county_map <- socviz::county_map %>% rename(fips = id)

## PUT TOGETHER
county_graph <- county_map %>% left_join(filter(usa_county_corona, date == max(date, na.rm = T)), 
                                         by = "fips")

## GET COUNTY NAMES FOR ALL FIPS (some missing in NYT)

url <- 'https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt'
response <- read_lines(url, skip = 72)

fips <- response %>% str_split('        ') %>% map(1) %>% flatten_chr()
county <- response %>% str_split('        ') %>% map(2) %>% flatten_chr()

fips_to_county <- data.frame(fips, county) %>%
  mutate_all(str_replace_all, ' {1,}', '') %>% #removes all extra spaces
  mutate(county = str_replace_all(county, 'County|Parish|Area|Borough|Census', ''))

## JOINING DATA
county_graph <- fips_to_county %>% 
  left_join(county_map) %>% 
  left_join(filter(usa_county_corona, date == max(date, na.rm = T)), 
            by = "fips") %>%
  filter(!is.na(lat))

## PLOT GRAPH
p <- county_graph %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  #filter(state != 'Alaska') %>%
  mutate(Location = paste(county, ", ", state, '\n', 
                          'Confirmed cases:', cases, sep = '')) %>%
  ggplot(aes(x = long, y = lat, fill = cases, group = fips, label = Location))  + 
  geom_polygon(color = NA) + #no border colors 
  coord_equal() +
  scale_fill_continuous(
    low = "white", #white or #f7f1be
    high = "#f2161d",
    na.value = "white",
    trans = 'log',
    labels = c('', '20', '400', '8000'), #manual
  ) +
  guides(fill = guide_legend(nrow = 1)) + #breaks down legend
  labs(x= '', y ='', fill = "Total Confirmed Cases",
       title = paste('COVID-19 Spread: Confirmed Cases,', max(county_graph$date, na.rm = T))
  ) +
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "bottom")

p %>% ggplotly(tooltip = 'Location')


county_map %>%
  ggplot(aes(x = long, y = lat, fill = group, group = group))  + 
  geom_polygon(color = 'grey') + #no border colors 
  coord_equal() +
  guides(fill = F) +
  labs(x= '', y ='', fill = "Total Confirmed Cases",
       title = paste('COVID-19 Spread: Confirmed Cases,', max(county_graph$date, na.rm = T))
  ) +
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "bottom")

## Add county name, number of cases, deaths to ploty label
## remove color

