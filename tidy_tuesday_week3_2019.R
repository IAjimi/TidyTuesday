library("readr")
library("ggrepel")
library("tidyverse")

launches <- read_csv("launches.csv")

### Country Launches Over Time ####
top_launches <- launches %>%  group_by(state_code) %>% count() %>% arrange(desc(n))
#finding out which countries launch the most -- China, United States, USSR / Russia

cold_war <- launches %>% filter(state_code %in% top_launches$state_code[c(1:4)]) %>%
  group_by(launch_year, state_code) %>% count()

ggplot(cold_war, aes(launch_year, n, color = state_code, label = state_code)) + 
  geom_path() + 
  geom_text_repel(data = subset(cold_war, launch_year == 2017), size = 4) +
  guides(color = FALSE, label = FALSE) +
  labs(x = "Year", y = "Number of Launches",
       title = "Space Race: US, USSR/RUS & CHN Launches Over Time")


### SUCCESS RATE ####
launches$success <- NA
launches$success[launches$category == "O"] <- 1
launches$success[launches$category == "F"] <- 0

launch_success <- launches %>%
  group_by(launch_year, state_code) %>% summarize(success = mean(success))

ggplot(launch_success, aes(launch_year, success)) + 
  geom_path(color = "grey") + 
  geom_path(data = subset(launch_success, state_code == "US"), color = "red", size = 1.5) +
  annotate("text", x = 1986, y = 0.64, label = "Challenger", color = "red", size = 4)+
  guides(color = FALSE, label = FALSE) +
  labs(x = "Year", y = "Success Rate",
       title = "United States' Launch Success Rate")
