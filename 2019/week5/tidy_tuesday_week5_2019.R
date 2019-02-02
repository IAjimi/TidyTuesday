library(tidyverse)

## Reading Data Files
milkcow_facts <- read_csv("C:/Users/ia767/Downloads/milkcow_facts.csv")
fluid_milk_sales <- read_csv("C:/Users/ia767/Downloads/fluid_milk_sales.csv")
milk_products <- read_csv("C:/Users/ia767/Downloads/milk_products_facts.csv")
cheese <- read_csv("C:/Users/ia767/Downloads/clean_cheese.csv")

### QUICK FUNCTION ####
#so the color labels will look nice later on
nice_labels <- function(x){
  x <- str_replace_all(x, "_", " ")
  x <- str_replace(x, "[o]ther", "(Other)") 
  x <- str_to_title(x)
  return(x)
}

### MILK COW FACTS ####
#adding milk consumption facts
milkcow_facts <- left_join(milkcow_facts, select(milk_products, year, fluid_milk), by = "year")
milkcow_facts <- dplyr::rename(milkcow_facts, "fluid_milk_consumption_(lbs)" = "fluid_milk")

#putting everything in % change
milk_change <- milkcow_facts %>%
  mutate_if(str_detect(colnames(.), "_"), #for columns with underscore
            funs(change = ( (. - lag(.)) / lag(.) ) ))  #get % yearly change
#this allows us to compare what happened over time to variables measured in 
#vastly different units

### source :https://stackoverflow.com/questions/46607352/using-group-by-with-mutate-if-by-column-name
### https://stackoverflow.com/questions/46607352/using-group-by-with-mutate-if-by-column-name

#Changing format to long
milk_change_long <- milk_change %>% gather(cow_var, unit, -year)

#Extended Plot 1 : Explaining Increase in Milk Production
key_cow_new <- c("avg_milk_cow_number_change","milk_per_cow_change" , "milk_production_lbs_change")

milk_change_long %>% filter(cow_var %in% key_cow_new) %>% ggplot(aes(year, unit)) + 
  geom_line() + facet_wrap(~ nice_labels(cow_var)) +
  scale_y_continuous(labels= scales::percent) +
  labs(title = "Total Production of Milk Increased Due To Higher Output Per Cow",
       subtitle = "Steady Increases in Milk Per Cow Has Enabled Milk Production to Keep Rising,
       Despite Decrease in Av. Number of Cows Per Farm",
       x = "", y = "Yearly % Change")

#Extended Plot 2: Explaining Increase in Milk Prices
key_cow2_new <- c("avg_price_milk_change", "milk_cow_cost_per_animal_change" , 
                  "milk_production_lbs_change", "fluid_milk_consumption_(lbs)_change")

milk_change_long %>% filter(cow_var %in% key_cow2_new) %>% ggplot(aes(year, unit)) + 
  geom_line() + facet_wrap(~ nice_labels(cow_var)) +
  scale_y_continuous(labels= scales::percent) +
  labs(title = "Rising Cost of Cows Has Kept Upward Pressure on Milk Prices",
       subtitle = "Even As Total Production Has Considerably Increased",
       x = "", y = "Yearly % Change")

#Extended Plot 3: Correlation Price/Cost
milk_change_long %>% 
  filter(cow_var %in% c("avg_price_milk_change", "milk_cow_cost_per_animal_change")) %>% 
  ggplot(aes(year, unit, color = nice_labels(cow_var))) + 
  geom_line() + 
  scale_y_continuous(labels= scales::percent) +
  labs(title = "Rising Cost of Cows Has Kept Upward Pressure on Milk Prices",
       subtitle = "Even As Total Production Has Considerably Increased",
       x = "", y = "Yearly % Change", color = "")


### MILK PRODUCTION ####
#Getting Most Produced Types of Milk (for relevance)
key_fluid_type <- fluid_milk_sales %>% filter(year == 2015 & milk_type != "Total Production") %>% 
  top_n(4)
key_fluid_type <- key_fluid_type$milk_type

### Plot 1: Total Production Per Type
fluid_milk_sales %>% filter(milk_type %in% key_fluid_type) %>% 
  ggplot(aes(year, pounds/1000000, color = milk_type)) + geom_line() +
  labs(title = "Whole Milk On The Rise Again",
       subtitle = "Whole Milk Consumption Close To Overtaking That of 2% Milk",
       x = "", y = "Pounds (Millions)", color = "Milk Type",
       caption = "Source: USDA (United States Department of Agriculture)")

### Plot 2: As a % of Total Production
#some df formatting to get put`Total Production` in a column of its own
temp_long <- spread(fluid_milk_sales, milk_type, pounds)
per_milk <- gather(temp_long, milk_type, pounds, -c(year, `Total Production`))
per_milk <- mutate(per_milk, per_prod = pounds / `Total Production`)

#plotting proper
per_milk %>% filter(milk_type %in% key_fluid_type) %>% 
  ggplot(aes(year, per_prod, color = milk_type)) + geom_line() +
  scale_y_continuous(labels= scales::percent) +
  labs(title = "Whole Milk On The Rise Again",
       subtitle = "Whole Milk Consumption Close To Overtaking That of 2% Milk",
       x = "", y = "% of Total Production", color = "Milk Type",
       caption = "Source: USDA (United States Department of Agriculture)")


### MILK CONSUMPTION ####
#putting df in long format
milk_products <- gather(milk_products, product_type, lbs, -year)

#selecting most consumed milk products
key_product_type <- milk_products %>% filter(year == 2015) %>% top_n(5)
key_product_type <- key_product_type$product_type

#plot 3
milk_products %>% filter(product_type %in% key_product_type) %>%
  ggplot(aes(year, lbs, color = nice_labels(product_type))) + 
  geom_line() +
  labs(title = "Fluid Milk Most Consumed Milk Product By Far",
       x = "", y = "Av. Consumption in Lbs Per Person", color = "Product",
       caption = "Source: USDA (United States Department of Agriculture)")

#plot 4 : Getting rid of Fluid milk
milk_products %>% filter(product_type %in% key_product_type & 
                           product_type != "fluid_milk") %>%
  ggplot(aes(year, lbs, color = nice_labels(product_type))) + 
  geom_line() +
  labs(title = "Changing Consumption Habits",
       subtitle = "Steady Increase in Consumption of Non-American Cheese & Yogurt",
       x = "", y = "Av. Consumption in Lbs Per Person", color = "Product",
       caption = "Source: USDA (United States Department of Agriculture)")


#### CHEESE CONSUMPTION ####
cheese_select <- cheese[, -c(which(str_detect(names(cheese), "Total") == TRUE))]
#removes columns incl "total' in name

cheese_long <- gather(cheese_select, cheese_type, lbs, -Year)

key_cheese_type <- cheese_long %>% filter(Year == 2015) %>% top_n(5)
key_cheese_type <- key_cheese_type$cheese_type

#plot 5
cheese_long %>% filter(cheese_type %in% key_cheese_type) %>% 
  ggplot(aes(Year, lbs, color = nice_labels(cheese_type))) + geom_line() +
  labs(title = "Changing Consumption Habits",
       subtitle = "Mozzarella and Cheddar Most Popular Cheeses",
       x = "", y = "Av. Consumption in Lbs Per Person", color = "Product",
       caption = "Source: USDA (United States Department of Agriculture)")
