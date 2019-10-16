library("tidyverse")

## Loading Data
Squirrel_Census <- read_csv("C:/Users/ia767/Downloads/2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv")

## Pre-Plotting Steps
Squirrel_Census <- Squirrel_Census %>%
  filter(!is.na(`Primary Fur Color`)) %>%
  rename(fur = `Primary Fur Color`)

## Fur Plot
Squirrel_Census %>% 
  mutate(
    colourin = case_when(
      fur == "Gray" ~ "grey",
      fur == "Black" ~ "black",
      fur == "Cinnamon" ~ "#d2691e")
  ) %>%
  ggplot(aes(X, Y, color = colourin)) + 
  geom_point(alpha = 0.5) + 
  scale_color_identity("Fur Color", #name of legend
                       guide = "legend", #adding legend
                       labels = c("Cinnamon", "Black", "Gray")) + #legend labels
  facet_grid(~ Shift) +
  labs(title = "Central Park Squirrels", 
       subtitle = "Location by Fur Color & Time of Day",
       x = "",  y = "",
       caption = "Source: Squirrel Census") 

## Proportions
#Age proportion
age_prop <- Squirrel_Census %>% 
  filter(Age %in% c("Adult", "Juvenile")) %>%
  group_by(Age) %>%
  count() %>%
  mutate(prop = n / sum(.$n))

#Fur graph
Squirrel_Census %>% 
  filter(Age %in% c("Adult", "Juvenile")) %>%
  group_by(Age, fur) %>%
  count() %>%
  mutate(prop = n / sum(.$n)) %>%
  left_join(age_prop, by = "Age") %>%
  mutate(age_prop = n.x / n.y,
         total = 1,
         colourin = case_when(
           fur == "Gray" ~ "grey",
           fur == "Black" ~ "black",
           fur == "Cinnamon" ~ "#d2691e")
  ) %>%
  ggplot(aes(Age, age_prop, fill = colourin)) + 
  geom_col() +
  coord_flip() +
  scale_fill_identity("Fur Color", #name of legend
                      guide = "legend", #adding legend
                      labels = c("Cinnamon", "Black", "Gray")) + #legend labels
  labs(title = "Primary Fur Colors by Age",
       subtitle = "Grey Squirrel Most Common, Black Least",
       x= "", y = "")


## Behavior
#Fur
Squirrel_Census %>% 
  group_by(fur) %>% 
  summarise_if(is.logical, mean) %>%
  gather(behavior, val, -fur) %>%
  mutate(
    colourin = case_when(
      fur == "Gray" ~ "grey",
      fur == "Black" ~ "black",
      fur == "Cinnamon" ~ "#d2691e")
  ) %>%
  filter(behavior != "Moans") %>% #empty category
  ggplot(aes(fur, val, fill = colourin)) + 
  geom_col() +
  coord_flip() +
  scale_fill_identity() +
  ylim(0, 0.55) +
  labs(x= "", y = "", title = "Squirrel Behavior by Primary Fur Color") +
  facet_wrap(~ behavior) 

#Fur and Age
Squirrel_Census %>% 
  filter(Age %in% c("Adult", "Juvenile")) %>%
  group_by(Age, fur) %>% 
  summarise_if(is.logical, mean) %>%
  gather(behavior, val, -Age, -fur) %>%
  mutate(
    colourin = case_when(
      fur == "Gray" ~ "grey",
      fur == "Black" ~ "black",
      fur == "Cinnamon" ~ "#d2691e")
  ) %>%
  mutate(category = paste(Age, fur)) %>% 
  filter(behavior != "Moans") %>% #empty category
  ggplot(aes(category, val, fill = colourin)) + 
  geom_col() +
  scale_fill_identity() +
  coord_flip() +
  ylim(0, 0.55) +
  labs(x= "", y = "", title = "Squirrel Behavior by Primary Fur and Age Color") +
  facet_wrap(~ behavior)
