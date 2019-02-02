library("readr")
library("ggrepel")
library("tidyverse")

tv_ratings <- read_csv("IMDb_Economist_tv_ratings.csv")

#### The Economist's Graph ####
#Basic Graph
ggplot(tv_ratings, aes(date, av_rating)) + geom_jitter(aes(size = share), alpha = 0.75) +
  geom_smooth(se = FALSE, method = "lm", size = 1.5, alpha = 0.65, color = "orange") +
  labs(x = "", y = "Average Rating", title = "A New Golden Age of Television?", 
       subtitle = "American TV Dramas' Mean IMBD User Rating Increased Over Time", 
       caption = "Size Weighted by Ratings Share") +
  guides(size = FALSE)

#Highlighting Specific Shows
key_titles <- c("Quantum Leap", "The Wire", "Sex and the City", "Game of Thrones",
                "Black Mirror")
dates <- as.Date(c("1991-01-24", "2003-07-12", "1998-07-14", "2016-05-25", "2017-12-29"))

ggplot(tv_ratings, aes(date, av_rating)) + 
  geom_jitter(aes(size = share), color = "grey", alpha = 0.5) + #base points
  geom_jitter(data = subset(tv_ratings, title %in% key_titles), #featured shows points
              aes(date, av_rating, size = share, color = title), alpha = 0.85) +
  geom_line(data = subset(tv_ratings, title %in% key_titles), #connecting lines for feat shows
            aes(date, av_rating, group = title, color = title), alpha = 0.85, size = 1.5) + 
  geom_smooth(se = FALSE, method = "lm", size = 1.25, alpha = 0.65, color = "orange") + #trend line
  geom_text_repel(data = subset(tv_ratings, title %in% key_titles & date %in% dates), #labels for feat shows
                  aes(date, av_rating, group = title, color = title, label = title), 
                  size = 3.5, fontface = "bold") +
  labs(x = "", y = "Average Rating", title = "A New Golden Age of Television?", 
       subtitle = "New Dramas Driving Prestige of TV as a Medium", 
       caption = "Size Weighted by Ratings Share", color = "Show") +
  guides(size = FALSE)
