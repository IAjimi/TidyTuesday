library(tidyverse)
library(ggrepel)

global_mortality <- read_excel("C:/Users/ia767/Downloads/global_mortality.xlsx")
global_mortality <- global_mortality %>% filter(!is.na(country_code)) 

## Worldwide Impact of Drug Disorders on Mortality ####
## USA only
ggplot(global_mortality, aes(year, `Drug disorders (%)`)) + 
  geom_line(aes(group = country_code), color = "grey") + 
  geom_line(data = subset(global_mortality, country_code == "USA"), 
            aes(group = country_code), color = "red", size = 1) + 
  geom_text_repel(data = subset(global_mortality, country_code == "USA" & year == 2016),
                  aes(label = country_code), size = 3.25, color = "red") +
  labs(x = "", title = "USA Leading in Drug Death Disorders", 
       subtitle = "Percentage of Deaths Related to Drug Disorders Per Country, 1991-2016",
       caption = "Source: OECD")

## Adding some countries with high mortality in the early 90s
colors_cc <- c("#fa8775", "#cd34b5", "#0000ff")

global_mortality %>% filter(year == 1990) %>% arrange(desc(`Drug disorders (%)`)) %>% 
  dplyr::select(country_code, `Drug disorders (%)`)

ggplot(global_mortality, aes(year, `Drug disorders (%)`)) + 
  geom_line(aes(group = country_code), color = "grey") + 
  geom_line(data = subset(global_mortality, country_code %in% c("RUS", "CHE", "USA")), 
            aes(group = country_code, color = country_code), size = 1) + 
  geom_text_repel(data = subset(global_mortality, country_code %in% c("RUS", "CHE", "USA") & year == 2016),
                  aes(label = country_code, color = country_code), size = 3.25) +
  scale_color_manual(values =  c("#fa8775", "#cd34b5", "#0000ff")) +
  guides(color = FALSE, label = FALSE) + 
  labs(x = "", title = "USA Leading in Drug Death Disorders", color = "Country",
       subtitle = "Percentage of Deaths Related to Drug Disorders Per Country, 1991-2016",
       caption = "Source: OECD")

## Locating Drug Deaths Within US Context ####
us_deaths <- global_mortality %>% filter(country_code == "USA") %>% gather(cause, deaths, 4:35) %>%
  mutate(change = deaths - lag(deaths)) %>% filter(year > 1991)

#overall change in cause of deaths -- drug disorders
ggplot(us_deaths, aes(year, change)) + 
  geom_line(aes(group = cause), color = "grey") + 
  geom_line(data = subset(us_deaths, cause == "Drug disorders (%)"), 
            color = "red", size = 1) + 
  geom_text_repel(data = subset(us_deaths, cause == "Drug disorders (%)" & year == 2016),
                  aes(x = 2016, y = -0.065, label = cause, color = cause), 
                  color = "red", size = 3.5) +
  ylim(c(-0.225, 0.225)) +
  labs(x = "",  y = "Yearly Change", 
       title = "Deaths Related to Drug Disorders on the Rise", color = "Country",
       subtitle = "Change in Cause of Deaths in U.S., 1991-2016",
       caption = "Source: OECD")

#steep decrease in cardiovascular deaths
us_deaths %>% group_by(cause) %>% summarize(change = mean(change)) %>% arrange(change)

ggplot(us_deaths, aes(year, change)) + 
  geom_line(aes(group = cause), color = "grey") + 
  geom_line(data = subset(us_deaths, cause %in% c("HIV/AIDS (%)", "Cardiovascular diseases (%)")), 
            aes(color = cause), size = 1) + 
  geom_text_repel(data = subset(us_deaths, 
                                cause %in% c("HIV/AIDS (%)", "Cardiovascular diseases (%)") & 
                                  year == 2012),
                  aes(label = cause, color = cause), size = 3.5) + guides(color = FALSE, label = FALSE) +
  scale_color_manual(values =  c("#0000ff", "#cd34b5")) +
  ylim(c(-0.75, 0.75)) +
  labs(x = "",  y = "Yearly Change", 
       title = "Sharp Decrease in HIV & Cardiovascular Diseases Deaths",
       subtitle = "Change in Cause of Deaths in U.S., 1991-2016",
       caption = "Source: OECD")
