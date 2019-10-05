pizza_barstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")

## Loading Libraries ####
library(leaflet)
library(tidyverse)

# Creating color palette + suitable names ####
pizza_barstool <- pizza_barstool %>% mutate(colourin = case_when(
  review_stats_all_average_score >= 8 ~ "#003399",
  T ~ "lightgray"),
  label_name = paste(name, "Score:", round(review_stats_all_average_score, 2))
)

# Leaflet map ####
leaflet() %>%
  addTiles() %>%
  setView(-73.96, 40.73, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~longitude, ~latitude, popup = ~as.character(label_name), 
                   label = ~as.character(round(review_stats_all_average_score, 2)), 
                   color = ~(colourin), data = pizza_barstool)

## City Ranking ####
city_ranking <- pizza_barstool %>% 
  group_by(city) %>% 
  summarise(review_community = mean(review_stats_community_average_score, na.rm = TRUE),
            review_critic = mean(review_stats_dave_average_score, na.rm = TRUE),
            n = n()) %>%
  filter(n >= 5) %>% #removing cities with too few reviews
  arrange(desc(review_critic))

city_ranking %>%
  ungroup() %>%
  top_n(7, review_critic) %>% #only keep top 7
  ggplot(aes(fct_reorder(city, review_critic), review_critic, label = round(review_critic, 2))) +
  geom_col() +
  geom_text(col = "white", size = 8, nudge_y = -0.75) + #add review on top of columns
  coord_flip() + 
  ylim(0, 10) + #show the rating is out of 10 max
  labs(x = "", y = "Average Critic Score", title = "Brooklyn #1 Rated American City for Pizza",
       caption = "Source: Barstool Sports")

## Community and Critic Review Gap ####
pizza_barstool %>% filter(review_stats_community_average_score > 0) %>%
  ggplot(aes(review_stats_dave_average_score, review_stats_community_average_score)) + 
  geom_jitter() + 
  geom_abline(intercept  = 0, slope = 1, color = "red", size = 1) +
  labs(x = "Critic Rating", y = "Community Rating", 
       title = "Critics tend to rate restaurants lower than ordinary restaurant-goers")

# for loop #
top_pizza <- data.frame("name" = c(NA), "city" = c(NA), "review"= c(NA), "price" = c(NA))

## Random Code 
#find top 3 pizzeria for every city
for (i in unique(pizza_barstool$city)){
  top_pizza <- filter(pizza_barstool, city == i) %>% 
    top_n(3, review_stats_all_average_score) %>%
    select(name, city, review_stats_all_average_score, price_level) %>% 
    rename(review = review_stats_all_average_score, price = price_level) %>%
    rbind(top_pizza, .)
}


