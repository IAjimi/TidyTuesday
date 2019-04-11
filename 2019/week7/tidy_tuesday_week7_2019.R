library(tidyverse)

#loading data in
fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")

#creating rnd as percentage of gdp column
fed_rd <- fed_rd %>% mutate(rd_per_gdp = rd_budget/gdp) 

#decade dummy
fed_rd$decade <- NA
fed_rd$decade[fed_rd$year >= 1960 & fed_rd$year <= 1970] <- "60s"
fed_rd$decade[fed_rd$year >= 1970 & fed_rd$year <= 1980] <- "70s"
fed_rd$decade[fed_rd$year >= 1980 & fed_rd$year <= 1990] <- "80s"
fed_rd$decade[fed_rd$year >= 1990 & fed_rd$year <= 2000] <- "90s"
fed_rd$decade[fed_rd$year >= 2000 & fed_rd$year <= 2020] <- "00s"

#finding departments with highest rnd percentage
fed_rd %>% group_by(department) %>% summarize(av_rd = mean(rd_per_gdp)) %>% 
  arrange(desc(av_rd)) %>% top_n(3)

##more dummies and reordering
fed_rd <- fed_rd %>%  mutate(
  filling = case_when( #creates dummy for color scheme
    department == "DOD" ~ "DOD",
    department == "HHS" ~ "HHS",
    department == "NIH" ~ "NIH",
    T ~ "Other"),
  decade = fct_reorder(decade, year),
  department = fct_reorder(department, rd_per_gdp)
)

#line plot 
ggplot(fed_rd, aes(year, rd_per_gdp, group = department, color = filling)) + 
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("#003399", "#FF2B4F", "#fcab27", "#777477")) +
  labs(title = "R&D Spending Per Year", 
       subtitle = "Department of Defense on the Decline",
       x = "",  y = "% of GDP",
       color = "Department") 

## Bar Chart
ggplot(fed_rd, aes(decade, rd_per_gdp, fill = filling)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#003399", "#FF2B4F", "#fcab27", "#777477")) +
  labs(title = "R&D Spending per Decade", 
       subtitle = "DoD Biggest Spender By Far",
       x = "",  y = "% of GDP",
       fill = "Department",
       caption = "Source: seattle.gov") 
