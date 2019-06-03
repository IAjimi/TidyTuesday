library(tidyverse)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

## average rating per country ####
overall <- ramen_ratings %>% group_by(country) %>% 
  summarize(all_ratings = mean(stars, na.rm = TRUE), n = sum(!is.na(stars))) %>% 
  filter(n >= 30) %>% #removing countries w low number of revies
  arrange(desc(all_ratings)) %>%
  top_n(8, all_ratings)

## average rating per country, bowl only
bowl <- ramen_ratings %>% filter(style == "Bowl" & country %in% overall$country) %>% 
  group_by(country) %>% 
  summarize(bowl_ratings = mean(stars, na.rm = TRUE), n = sum(!is.na(stars))) %>% 
  arrange(desc(bowl_ratings))

bowl$bowl_ratings[bowl$n <= 30] <- NA #removing ratings below 20 reviews

## creating labels for plot
plot_labels <- c(all_ratings = "Overall Ratings", 
                   bowl_ratings = "Bowl Ratings") 

## creating plot
inner_join(overall, bowl, by = "country") %>%  #merging previous df
  gather(rating_type, rating, c("all_ratings", "bowl_ratings")) %>% #gathering to use facet_grid()
  mutate(
    colourin = case_when(
      country == "Japan" ~ "#003399",
      T ~ "darkgray")
  ) %>%
  ggplot(aes(fct_reorder(country, rating), rating, fill = colourin)) + 
  geom_col() + 
  coord_flip() +
  scale_fill_identity(guide = FALSE) + #no labels
  facet_grid(~ rating_type, labeller= labeller(rating_type = plot_labels)) + 
  ylim(0, 5) + #since scale is from 0 -5
  labs(title = "Ramen Ratings", 
       subtitle = "Average Country Score on 0-5 Scale",
       x = "",  y = "",
       caption = "Countries With 30+ Reviews Per Category") 

### japan's rating distribution relative to mean ####
#top styles
top_styles <- ramen_ratings %>% group_by(style) %>% count() %>% arrange(desc(n))
top_styles <- top_styles$style[c(1:3)] #top 3 styles

# country + style average
japan_country_style <- ramen_ratings %>% 
  filter(style %in% top_styles & country == "Japan") %>%
  group_by(country, style) %>% 
  summarize(rating = mean(stars, na.rm = TRUE))

# style average
country_style <- ramen_ratings %>% 
  filter(style %in% top_styles) %>%
  group_by(style) %>% 
  summarize(rating = mean(stars, na.rm = TRUE))


## plot 2
ramen_ratings %>% 
  filter(style %in% top_styles) %>% 
  ggplot(aes(stars)) + 
  geom_density(fill = "gray20", color = FALSE, alpha = 0.9, aes(y = ..scaled..)) +
  geom_vline(data = japan_country_style, aes(xintercept = rating), color = "red", size = 0.85) +
  geom_text(data = japan_country_style, aes(x = rating, y = 0.2, label = country),
            nudge_x = 0.45, color = "red", size = 3.5, hjust = 1,
            fontface = "bold") +
  geom_vline(data = country_style, aes(xintercept = rating), color = "white", size = 0.85) +
  geom_text(data = country_style, aes(x = rating, y = 0.2, label = "Average"),
            nudge_x = -0.05, color = "white", size = 3.5, hjust = 1,
            fontface = "bold") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + #removing y axis labels
  facet_wrap(~ style, ncol = 1) +
  labs(x = "Score", y = NULL, 
       title = "Ramen Ratings Per Style",
       subtitle = "Japan's Ratings Distribution Relative to the Mean")
