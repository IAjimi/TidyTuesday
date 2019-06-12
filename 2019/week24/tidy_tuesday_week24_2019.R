library(tidyverse)

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

### number meteorites ###
## per fall type
meteorites %>% filter(year >= 1975 & year <= 2019) %>% 
  group_by(year, fall) %>% count() %>% ggplot(aes(year, n, fill = fall)) + 
  geom_col(alpha = 0.9) +
  scale_fill_manual(values = c("#db4551", 'darkgrey')) +
  labs(title = "Observed Meteorites", 
       subtitle = "Number of Meteorites Found or Fallen per Year, 1975-2019",
    x = "", y = "Number of Meteorites", fill = "", 
    caption = "Source: The Meteoritical Society")

## per class
top_class <- meteorites %>% filter(year >= 1975 & year <= 2019) %>% 
  group_by(class) %>% count() %>% arrange(desc(n))
top_class <- top_class$class[1:4]

meteorites %>% filter(year >= 1975 & year <= 2019) %>% 
  group_by(year, class) %>% 
  count() %>% 
  mutate(
    colourin = case_when(
      class == "L6" ~ "#003399",
      class == "H5" ~ "#FF2B4F",
      class == "L5" ~ "#fcab27",
      class == "H6" ~ "#3686d3",
      T ~ "darkgray")
  ) %>%
  ggplot(aes(year, n, fill = colourin)) + 
  geom_col() + 
  scale_fill_identity("Class", #name of legend
                      guide = "legend", #adding legend
                      labels = c("L6", "H5", "L5", "H6", "Other")) + #legend labels
  labs(title = "Observed Meteorites", 
       subtitle = "Number of Meteorites Found or Fallen per Year, 1975-2019",
       x = "", y = "Number of Meteorites", fill = "", 
       caption = "Source: The Meteoritical Society")
  

### World Map ####
#partly from https://stackoverflow.com/questions/30706124/plotting-the-world-map-in-r
WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  geom_point(data = filter(meteorites, year >= 1950 & year <= 2019), 
             aes(long, lat, size = mass*0.2, color = fall), alpha = 0.6) +
  scale_color_manual(values = c("#db4551", '#455edb')) +
  guides(size = FALSE) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(title="Meteorite Impact", subtitle = "Meteorites Fallen and Found Between 1950 - 2019",
       x="", y="", color = "", caption = "Source: The Meteoritical Society") +
  theme_bw()

### per class
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  geom_point(data = meteorites %>% filter(year >= 1950 & year <= 2019 & fall == "Fell") %>% 
               mutate(
                 colourin = case_when(
                   class == "L6" ~ "#003399",
                   class == "H5" ~ "#FF2B4F",
                   class == "L5" ~ "#fcab27",
                   class == "H6" ~ "#3686d3",
                   T ~ "darkgray")
               ) ,
             aes(long, lat, size = mass*0.2, color = colourin), alpha = 0.7) +
  guides(size = FALSE) +
  scale_color_identity("Class", #name of legend
                      guide = "legend", #adding legend
                      labels = c("L6", "H5", "L5", "H6", "Other")) + #legend labels
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(title="Meteorite Impact", subtitle = "Meteorites Fallen Between 1950 - 2019",
       x="", y="", color = "Class", caption = "Source: The Meteoritical Society") +
  theme_bw()
