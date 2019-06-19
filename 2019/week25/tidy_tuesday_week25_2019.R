library("tidyverse")
library(ggrepel)

bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

## finding dominant species
top_birds <- bird_counts %>% group_by(species) %>% 
  summarize(count_per_hour = mean(how_many_counted_by_hour, na.rm = TRUE)) %>%
  arrange(desc(count_per_hour)) 
top_3_birds <- top_birds$species[1:3]
top_5_birds <- top_birds$species[1:5]

## Graph 1: Starlings vs others
bird_counts %>% filter(year >= 1950) %>%
  ggplot(aes(year, how_many_counted_by_hour)) + 
  geom_line(aes(group = species), color = "grey", alpha = 0.65) +
  geom_line(data = . %>% filter(species %in% top_3_birds),
            aes(color = species)) +
  geom_text_repel(data = . %>% filter(species == "Herring Gull" & year == 1955),
                  aes(color = species, label = species)) +
  geom_text_repel(data = . %>% filter(species == "European Starling" & year == 1980),
                  aes(color = species, label = species)) +
  geom_text_repel(data = . %>% filter(species == "Canada Goose" & year == 2000),
                  aes(color = species, label = species)) +
  scale_color_manual(values = c("#fcab27", "darkgreen", "#FF2B4F")) +
  labs(title = "Christmas Bird Counts",
       subtitles = "Species With The Greated Average Count Per Hour Over Time",
       x = "", y = "Count Per Hour", color = "Species")

## Plot 2: highlighting change, excluding starlings
most_change <- bird_counts %>% filter(year >= 1950) %>% 
  group_by(species) %>% 
  summarize(change = sd(how_many_counted_by_hour)) %>%
  arrange(desc(change)) %>%
  top_n(5)
most_change <- most_change$species

## in context: other species in background
bird_counts %>% filter(year >= 1950 & species != "European Starling") %>%
  ggplot(aes(year, how_many_counted_by_hour)) + 
  geom_line(aes(group = species), color = "darkgrey", alpha = 0.35) +
  geom_line(data = . %>% filter(species %in% most_change),
            aes(color = species)) +
  scale_color_manual(values = c("#fcab27", "#003399", "#FF2B4F", "#3686d3")) +
  labs(title = "Christmas Bird Counts",
       subtitles = "Species With Greatest Change In Count/Hour",
       x = "", y = "Count Per Hour", color = "Species")

## highlighting with facet_grid
bird_counts %>% filter(year >= 1950 & species != "European Starling" & species %in% most_change) %>%
  ggplot(aes(year, how_many_counted_by_hour)) + 
  geom_line(aes(color = species), alpha = 0.65) +
  scale_color_manual(values = c("#fcab27", "#003399", "#FF2B4F", "#3686d3")) +
  guides(color = FALSE) +
  facet_grid(~ species) +
  labs(title = "Christmas Bird Counts",
       subtitles = "Species With Greatest Change In Count/Hour",
       x = "", y = "Count Per Hour", color = "Species")

