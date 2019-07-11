library("tidyverse")

### Preparing Data
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

wine_ratings <- select(wine_ratings, -X1, -taster_name, -taster_twitter_handle) #cleaning up df
wine_ratings <- mutate(wine_ratings, ppp = points / price) #adding ppp measure

### Country level analysis ####
country_ratings <- wine_ratings %>% group_by(country) %>% summarize(n = sum(!is.na(price)),
                                                                    price = median(price, na.rm = TRUE),
                                                                    points = median(points, na.rm = TRUE),
                                                                    ppp = median(ppp, na.rm = TRUE)) %>% 
  arrange(desc(points))
#wanted to look at country with highest price, but data skewed by the fact that countries
#with very low number of reviews are included

#going to add an arbitrary 50 review minimum
country_ratings <- filter(country_ratings, n >= quantile(country_ratings$n, 0.7))

top_rated_value <- wine_ratings %>% filter(points >= 95) %>% 
  group_by(country) %>% 
  summarize(n = sum(!is.na(price)), price = median(price, na.rm = TRUE),
            points = median(points, na.rm = TRUE), ppp = median(ppp, na.rm = TRUE)) %>% 
  arrange(desc(ppp)) #picking countries for first plot

top_6_quantity <- country_ratings %>% arrange(desc(n)) %>% top_n(6, n) #picking countries for second plot

#plot
wine_ratings %>% 
  filter(country %in% country_ratings$country) %>%
  mutate(
    colourin = case_when(
      country == "Austria" ~ "#003399",
      country == "Portugal" ~ "#FF2B4F",
      country == "Argentina" ~ "#fcab27",
      T ~ "darkgray")
  ) %>%
  ggplot(aes(points, price, color = colourin)) + 
  geom_jitter(data = . %>% filter(!(country %in% c("Austria", "Portugal", "Argentina"))), 
              alpha = 0.2) + 
  geom_jitter(data = . %>% filter(country %in% c("Austria", "Portugal", "Argentina") ), 
              alpha = 0.7)+ 
  scale_color_identity("Country", #name of legend
                       guide = "legend", #adding legend
                       labels = c("Austria", "Argentina", "Portugal",  "Other")) + #legend labels
  labs(title = "Wine Prices", 
       subtitle = "Highlighting Countries with the Best Point/Price Ratio for 95+ Rated Wines",
       x = "Points",  y = "Price",
       caption = "Source: Kaggle") 


## Facet Grid
wine_ratings %>% 
  filter(country %in% top_6_quantity$country) %>%
  mutate(
    colourin = case_when(
      country == "Spain" ~ "#003399",
      country == "France" ~ "#FF2B4F",
      country == "Italy" ~ "#fcab27",
      T ~ "darkgray")
  ) %>%
  ggplot(aes(points, price, color = colourin)) + 
  geom_jitter(data = . %>% filter(!(country %in% c("Spain", "France", "Italy"))), 
              alpha = 0.2) + 
  geom_jitter(data = . %>% filter(country %in% c("Spain", "France", "Italy") ), 
              alpha = 0.7)+ 
  scale_color_identity("Country", #name of legend
                       guide = "legend", #adding legend
                       labels = c("Spain", "Italy",  "France",  "Other")) + #legend labels
  facet_grid(~ country) +
  labs(title = "Wine Prices", 
       subtitle = "Countries with the Most Reviews",
       x = "Points",  y = "Price",
       caption = "Source: Kaggle") 


### Variety/Country Analysis ####
variety_rating <- wine_ratings %>% group_by(variety, country) %>% summarize(n = sum(!is.na(price)),
                                                          price = median(price, na.rm = TRUE),
                                                          points = median(points, na.rm = TRUE),
                                                          ppp = median(ppp, na.rm = TRUE)) %>% 
  filter(n >= 50) 
#best wines ppp, with median points above average 

graph_varieties <- c("Riesling", "Chardonnay", "Pinot Noir") 
#varieties with color in graph

#plot
variety_rating %>% 
  mutate(
    colourin = case_when(
      variety == "Rosé" ~ "#003399", #most expensive
      variety == "Cabernet Sauvignon" ~ "#FF2B4F", #bad ppp
      variety == "Chardonnay" ~ "#fcab27",
      T ~ "darkgray")
  ) %>%
  ggplot(aes(points, price, color = colourin)) + 
  geom_jitter(data = . %>% filter(!(variety %in% graph_varieties)), 
              alpha = 0.4) + 
  geom_jitter(data = . %>% filter(variety %in% graph_varieties ))+ 
  scale_color_identity("Variety", #name of legend
                       guide = "legend", #adding legend
                       labels = c(graph_varieties, "Other")) + #legend labels
  labs(title = "Wine Variety Ratings", 
       subtitle = "Median Price & Points of Country-Variety Combinations",
       x = "Points",  y = "Price",
       caption = "Source: Kaggle") 


##number of reviews?
ggplot(variety_rating, aes(price, n)) + 
  geom_jitter(alpha = 0.4) + 
  labs(title = "Wine Variety Ratings", 
       subtitle = "Focusing on most popular varieties' ratings",
       x = "Price", y = "Number of Reviews", 
       caption = "Source: Kaggle") 
