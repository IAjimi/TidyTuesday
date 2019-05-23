library("tidyverse")

#importing data
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

#cleaning up column names
names(mismanaged_vs_gdp)[c(4:6)] <- c("per_capita_waste", "per_capita_gdp", "population")
names(waste_vs_gdp)[c(4:6)] <- c("per_capita_waste", "per_capita_gdp", "population")

#selecting countries of interest
key_countries <- c("China", "India", "Russia", "United States")

#looking at year data
noNA <- mismanaged_vs_gdp %>% filter(!is.na(per_capita_gdp) & !is.na(per_capita_waste))
unique(noNA$Year) #only one year that has gdp and waste data: 2010

#plots
## plot 1
ggplot(mismanaged_vs_gdp, aes(log(per_capita_gdp), log(per_capita_waste*1000),  size = population)) + 
  geom_point() +
  geom_point(data = filter(mismanaged_vs_gdp, 
                           Entity %in% key_countries),
                           aes(log(per_capita_gdp),  log(per_capita_waste*1000), 
                                size = population, color = Entity)) +
  guides(size = FALSE) +
  labs(title = "Mismanaged Plastic Waste & GDP",
       subtitle = "Richer Countries Manage Plastic Waste Better...",
    x = "Per Capita GDP (Log)", y = "Mismanaged Plastic Waste per Capita (Log)", color = "Country")

## plot 2
ggplot(waste_vs_gdp, aes(log(per_capita_gdp), log(per_capita_waste*1000),  size = population)) + 
  geom_point() +
  geom_point(data = filter(waste_vs_gdp, 
                           Entity %in% key_countries),
             aes(log(per_capita_gdp),  log(per_capita_waste*1000), 
                 size = population, color = Entity)) +
  guides(size = FALSE) +
  labs(title = "Total Plastic Waste & GDP",
       subtitle = "...But Produce More Overall Plastic Waste",
       x = "Per Capita GDP (Log)", y = "Plastic Waste per Capita (Log)", color = "Country")


