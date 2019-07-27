library(tidyverse)
library("HoRM")

data(JamesBond)

### Bond Movie Gross vs Profit ####
JamesBond %>%
  select(Year, US_Adj, World_Adj, Budget_Adj) %>%
  gather(metric, value, US_Adj:Budget_Adj) %>%
  mutate(
    colourin = case_when(
      metric == "World_Adj" ~ "#003399",
      metric == "Budget_Adj" ~ "#FF2B4F",
      metric == "US_Adj" ~ "#3686d3")
  ) %>%
  ggplot(aes(Year, value/1000, color = colourin)) + 
  geom_line() +
  scale_color_identity("", 
                       guide = "legend",
                       labels = c("World Gross", "U.S. Gross", "Budget")) +
  labs(title = "Global Audience Keeps Driving James Bond Profits",
       subtitle = "Movies Are Breaking Even In The U.S.",
       x = "",
       y = "($, mn)")


### Total Profits Per Bond ####
JamesBond %>%
  mutate(Total_Profit = US_Adj + World_Adj - Budget_Adj) %>%
  ggplot(aes(Year, Total_Profit/1000, label = Bond)) + 
  geom_line(color = "darkgrey") +
  geom_line(data = . %>% filter(Bond == "Sean Connery" & Year <= 1970), 
            color = "#003399", size = 0.5) +
  geom_line(data = . %>% filter(Bond == "Roger Moore"), 
            color = "#fcab27", size = 0.5) +
  geom_line(data = . %>% filter(Bond == "Pierce Brosnan"), 
            color = "#3686d3", size = 0.5) +
  geom_line(data = . %>% filter(Bond == "Daniel Craig"), 
            color = "#FF2B4F", size = 0.5) +
  geom_text(data = . %>% filter(Year == 1967), 
            color = "#003399", nudge_x = 3, nudge_y = 500) +
  geom_text(data = . %>% filter(Year == 1977), 
            color = "#fcab27", nudge_x = 2, nudge_y = 80) +
  geom_text(data = . %>% filter(Year == 1997), 
            color = "#3686d3", nudge_x = 2, nudge_y = -80) +
  geom_text(data = . %>% filter(Year == 2012), 
            color = "#FF2B4F", nudge_y = 80) +
  labs(title = "Total Profits By James Bond Era",
       subtitle = "Most Successful Bonds were Sean Connery & Daniel Craig",
       x = "",
       y = "($, mn)")

###Kill or Be Killed####
JamesBond %>%
  mutate(
    colourin = case_when(
      Bond == "Sean Connery" ~ "#003399",
      Bond == "Daniel Craig" ~ "#FF2B4F",
      Bond == "Roger Moore" ~ "#fcab27",
      Bond == "Pierce Brosnan" ~ "#3686d3",
      T ~ "darkgray")
  ) %>%
  ggplot(aes(Kills_Bond, Kills_Others, color = colourin, label = Movie)) + 
  geom_point(size = 2) +
  geom_text(data = . %>% filter(Movie == "Spectre") ,
            nudge_y = 9) +
  geom_text(data = . %>% filter(Movie == "You Only Live Twice") ,
            nudge_y = -9) +
  scale_color_identity("",
                     guide = "legend",
                     labels = c("Sean Connery", "Pierce Brosnan", 
                                "Roger Moore", "Daniel Craig", "Others") ) +
  labs(title = "Killed or Be Killed",
       subtitle = "Cause of Death per James Bond Movie",
       x = "People Killed by Bond",
       y = "People Killed by Others")

#finding movies with most deaths
#JamesBond %>% select(Bond, Kills_Others, Movie) %>% arrange(desc(Kills_Others)) %>% head()

##Ratings####
JamesBond %>%
  mutate(
    colourin = case_when(
      Bond == "Sean Connery" ~ "#003399",
      Bond == "Daniel Craig" ~ "#FF2B4F",
      Bond == "Roger Moore" ~ "#fcab27",
      Bond == "Pierce Brosnan" ~ "#3686d3",
      T ~ "darkgray"),
    Total_Profit = US_Adj + World_Adj - Budget_Adj
  ) %>%
  ggplot(aes(Total_Profit/1000, Avg_User_Rtn_Tom, color = colourin, label = Movie)) + 
  geom_point(size = 2) +
  geom_text(data = . %>% filter(Movie == "Spectre"),
            nudge_x = 50, nudge_y = -0.35) +
  geom_text(data = . %>% filter(Movie == "Goldfinger"),
            nudge_y = 0.45) +
  scale_color_identity("",
                       guide = "legend",
                       labels = c("Sean Connery", "Pierce Brosnan", 
                                  "Roger Moore", "Daniel Craig", "Others") ) +
  ylim(0, 10) +
  labs(title = "Bond Ratings & Profit",
       subtitle = "Movies with Higher Rotten Tomato User Rating Tend To Do Better",
       x = "Total Profit (mn $)",
       y = "Average Rating")

## Average Rating / Actor ####
JamesBond %>% 
  group_by(Bond) %>%
  summarise(rating = mean( (Avg_User_IMDB + Avg_User_Rtn_Tom)/2 ),
            time = mean(Year)) %>%
  mutate(
    colourin = case_when(
      Bond == "Sean Connery" ~ "#003399",
      Bond == "Daniel Craig" ~ "#FF2B4F",
      Bond == "Roger Moore" ~ "#fcab27",
      Bond == "Pierce Brosnan" ~ "#3686d3",
      T ~ "darkgray")
  ) %>%
  ggplot(aes(fct_reorder(Bond, time), rating, fill = colourin)) +
  scale_fill_identity() +
  coord_flip() +
  geom_col() +
  ylim(0, 10) +
  labs(title = "Battle of the Bonds",
       subtitle = "Sean Connery & Daniel Craig Neck-to-Neck for Highest Rated Bond Movies",
    x= "", y= "Average Movie Rating")

            