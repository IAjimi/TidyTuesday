library(readr)
library(tidyverse)

movie_profit <- read_csv("Coding/movie_profit.csv")


### PROMPT #### 
### x axis: multiple of budget
### y axis: percentage of films by GENRE w positive profit

#maintenance
movie_profit$release_date <- mdy(movie_profit$release_date)
movie_profit <- filter(movie_profit, release_date >= "1994-01-01")

#1 create profit variable
movie_profit <- movie_profit %>% 
  mutate(profit = (domestic_gross + worldwide_gross) / production_budget) %>%
  mutate(profit = round(profit, 2))

#2 create percentage variable  
N <-  movie_profit %>% group_by(genre) %>% count() #counting the number of movies per genre

genre_profit <- movie_profit %>% group_by(genre, profit) %>% count() 
#counting number movies per genre with given profit multiple

genre_profit$prop <- NA #creating proportion variable
for (i in c(1:nrow(genre_profit))) { 
  genre_profit$prop[i] <- genre_profit$n[i] / N$n[N$genre == genre_profit$genre[i]]
} #uses loop to calculate proportion using total stored in N

genre_profit %>% group_by(genre) %>% summarize(sum(prop)) #sanity check


### 3 creating the graph
#first graph
key_genre <- c("Horror", "Comedy", "Drama")
  
genre_profit %>% filter(profit >= 1 & profit <= 10 & genre %in% key_genre) %>%  
  ggplot(aes(profit, prop)) + geom_smooth(aes(color = genre), se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Profit Multiple", y = "Percent", title = "Horror Movies Most Profitable Genre",
       subtitle = "Return on Investment of Movies by Genre, 1994 - 2014", color = "Genre")

#second graph
genre_colors <- c(rep("#A9A9A9", 4), "#2E74C0")

genre_profit %>% filter(profit >= 1 & profit <= 10) %>%  
  ggplot(aes(profit, prop)) + geom_smooth(aes(color = genre), se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = genre_colors) +
  labs(x = "Profit Multiple", y = "Percentage of Movies", 
       title = "Horror Is Most Profitable Movie Genre",
       subtitle = "Return on Investment of Movies by Genre, 1994 - 2014", color = "Genre") +
  guides(color = FALSE)
