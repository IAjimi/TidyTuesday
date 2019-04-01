library(readr)
library(tidyverse)
library(lubridate)

###Loading Data####
pets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")
names(pets) <- c("date", "license_number", "name", "species", "breed", "breed_second", "ZIP")
pets$date <- as.Date(pets$date, "%B %d %Y")

###Dog or Cat City?####
#counting most popular breed
most_popular_breed <- pets %>% group_by(species) %>% 
  count() 
most_popular_breed <- most_popular_breed %>% mutate(prop = n / sum(most_popular_breed$n))

#changing proportions for graph clarity
most_popular_breed$graph_prop <- most_popular_breed$prop #graph prop equal to real prop

for (i in c(1:nrow(most_popular_breed)) ) { #unless it is too small to appear
  if (most_popular_breed$graph_prop[i] <= 0.01) {
    most_popular_breed$graph_prop[i] <- most_popular_breed$prop[i]*20 
    #this increases % -- not accurate but makes it visible on plot while giving sense of proportion
  } 
}


#graph
ggplot(most_popular_breed, aes(fct_reorder(species, graph_prop), graph_prop, fill = species)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +
  scale_fill_manual(values = c("#47af22", "orange", "#ea5f94", "#9d02d7")) +
  guides(fill = FALSE) +
  labs(title = "Seattle Officially A Dog City", 
       subtitle = "Goats and Pigs Also Welcome",
       x = "",  y = "% of Licenses",
       caption = "Source: seattle.gov") 

###Dog or Cat Name?####
#counting most names
pet_name <- pets %>% filter(!is.na(name)) %>% 
  group_by(name) %>% 
  count()

#counting most popular goat names
pet_name_goat <- pets %>% filter(!is.na(name) & species == "Goat") %>% 
  group_by(species, name) %>% 
  count() 

#most popular names per species (not only goats)
pet_name_species <- pets %>% filter(!is.na(name)) %>% 
  group_by(species, name) %>% 
  count() %>% filter(n >= 1 )

#merge most popular names + most popular names per species
pet_name_full <- left_join(pet_name_species, pet_name, by = "name", suffix = c("_species", "_total"))

#add column that shows proportion of species per name
pet_name_full <- mutate(pet_name_full, prop = n_species/n_total)

#graph
pet_name_full %>% filter(name %in% pet_name_goat$name & n_total >= 20) %>%
  ggplot(aes(fct_reorder(name, desc(n_species)), prop, fill = species)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +
  scale_fill_manual(values = c("#47af22", "orange", "#ea5f94")) +
  labs(title = "G.O.A.T. Pet Names", 
       x = "",  y = "% of Licenses", fill = "Species",
       caption = "Source: seattle.gov") 
