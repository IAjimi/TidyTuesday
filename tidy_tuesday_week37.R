library(tidyverse)
library(stringr)
library(lubridate)

nyc_restaurants <- read_csv("Coding/nyc_restaurants.csv")

nyc_restaurants$inspection_date <- mdy(nyc_restaurants$inspection_date)

## First Chart: Distribution of Grades per Number of Vermins ####
### 1 counting number vermins in violation description ####
### note: a shortcut would be to look up the violation code
vermin <- c("roaches", "live mice", "flies present", "live rats")
vermin_match <- str_c(vermin, collapse = "|")
nyc_restaurants <- mutate(nyc_restaurants, 
                          n_vermin = str_count(nyc_restaurants$violation_description, vermin_match))

###note: if just "mice" or "flies", will have several matches bc description uses term repeatedly

### 2 adding all vermin violations per restaurant ####
## dataset lists one violation at a time, not all violations per restaurant
vermin_violations <- nyc_restaurants %>% filter(!is.na(grade), !is.na(n_vermin)) %>% 
  dplyr::select(camis, inspection_date, grade, score, n_vermin) #focuses on variables of interest

vermin_violations <- vermin_violations %>% group_by(camis, grade, inspection_date) %>% 
  summarise(n_vermin = sum(n_vermin, na.rm = TRUE))

### 3 replacing that with a 0, 1, 2, 3+ system
vermin_violations$vermins <- NA

for (i in c(1:nrow(vermin_violations))) { #a bit slow so maybe not best idea
  if (vermin_violations$n_vermin[i] <= 2) {
    vermin_violations$vermins[i] <- as.character(vermin_violations$n_vermin[i])
  } else {
    vermin_violations$vermins[i] <- "3+"
  }
}

### 4 calculating proportions ####
vermin_graph <- vermin_violations %>%  filter(grade %in% c("A", "B", "C") 
                                              & inspection_date >= "01-01-2018") %>% 
  group_by(vermins) %>% count(grade) 

N <- vermin_violations %>%  filter(grade %in% c("A", "B", "C") & inspection_date >= "01-01-2018") %>% 
  group_by(vermins) %>% count() 

vermin_graph$prop <- NA #creating proportion variable
for (i in c(1:nrow(vermin_graph))) { 
  vermin_graph$prop[i] <- vermin_graph$n[i] / N$n[N$vermins == vermin_graph$vermins[i]]
} #uses loop to calculate proportion using total stored in N


### 5 making graph ####
vermin_graph %>% filter(grade %in% c("A", "B", "C")) %>%
  ggplot(aes(vermins, prop)) + geom_col(aes(fill = grade)) + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#47af22", "orange", "red")) +
  labs(title = "Restaurant Health Grades & Vermin Sightings", 
       subtitle = "Rats, Mice, Flies and Roaches Don't Stop NYC Restaurants From Getting As",
       x = "Number of Vermins",  y = "", 
       caption = "Source: NYC Department of Health, 2018") 
#numbers slightly different from Five Thirty-Eight bc no "other" category