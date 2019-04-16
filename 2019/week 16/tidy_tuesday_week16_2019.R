library(tidyverse)
library(ggrepel)

#replicating https://medium.economist.com/mistakes-weve-drawn-a-few-8cdd8a42d368

### Facebook Likes ####
corbyn <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/corbyn.csv")

ggplot(corbyn, aes(fct_reorder(political_group, avg_facebook_likes), avg_facebook_likes)) + 
  geom_col() + 
  coord_flip() +
  labs(title = "Left-Click", 
       subtitle = "Average number of likes per Facebook post, 2016, Thousands",
       x = "",  y = "",
       caption = "Source: Facebook") 

### Trade ####
trade <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/trade.csv")

labels <- c(manufacture_employment = "Manufacture Employment", 
            trade_deficit = "Trade Deficit") #creating plot labels

trade %>% mutate(trade_deficit = trade_deficit/1000000000,  #changing units
                 manufacture_employment = manufacture_employment/10000000) %>%
  gather(trade_stat, value, -year) %>% #gathering for facet plot
  ggplot(aes(year, value)) + 
  geom_line() + 
  facet_wrap(~ trade_stat, scales = "free", labeller= labeller( trade_stat = labels) ) + 
  labs(title = "Free markets and free workers",
       subtitle = "United States", 
       x = "", y = "Dollars (Bn)", 
       caption = "Sources: US Census Bureau, BLS")
#not ideal. the use of double axes means the problem that the original plot had still remains
#(manufacture employment is positive, trade def negative)


### Pensions ####
pensions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/pensions.csv")

key_nations <- c("Turkey", "Mexico", "Poland", "OECD Average", "United States",
                 "South Korea", "France", "Italy", "Greece", "Japan")

reg_outlier <- lm(gov_spend_percent_gdp ~ pop_65_percent, data = pensions) #regression for regression line
reg <- lm(gov_spend_percent_gdp ~ pop_65_percent, data = filter(pensions, country != "Brazil"))

ggplot(pensions, aes(pop_65_percent, gov_spend_percent_gdp, label = country)) + 
  geom_point(alpha = 0.25) +
  geom_point(data = filter(pensions, country %in% key_nations), alpha = 0.75) +
  geom_text_repel(data = filter(pensions, country %in% key_nations)) +
  geom_point(data = filter(pensions, country == "Brazil"), alpha = 0.75, color = "red") +
  geom_text_repel(data = filter(pensions, country == "Brazil"), color = "red") +
  labs(title = "Brazil's golden oldie blowout",
       subtitle = "Latest available",
       x = "% Population over 65", y = "% Gov Spending on Pensions",
       caption = "Sources: OECD, World Bank")


#with regression lines
reg_outlier <- lm(gov_spend_percent_gdp ~ pop_65_percent, data = pensions) #regression for regression line
reg <- lm(gov_spend_percent_gdp ~ pop_65_percent, data = filter(pensions, country != "Brazil"))

ggplot(pensions, aes(pop_65_percent, gov_spend_percent_gdp, label = country)) + 
  geom_point(alpha = 0.25) +
  geom_abline(intercept = unname(reg$coefficients[1]), slope = unname(reg$coefficients[2]),
              alpha = 0.25) + #all data regression line
  geom_abline(intercept = unname(reg_outlier$coefficients[1]), slope = unname(reg_outlier$coefficients[2]),
              alpha = 0.65, color = "red") + #OUTLIER regression line
  geom_point(data = filter(pensions, country %in% key_nations), alpha = 0.75) +
  geom_text_repel(data = filter(pensions, country %in% key_nations)) +
  geom_point(data = filter(pensions, country == "Brazil"), alpha = 0.75, color = "red") +
  geom_text_repel(data = filter(pensions, country == "Brazil"), color = "red") +
  labs(title = "Brazil's golden oldie blowout",
       subtitle = "Regression Lines With the Inclusion of Brazil",
       x = "% Population over 65", y = "% Gov Spending on Pensions",
       caption = "Sources: OECD, World Bank")

### EU Budget ####
eu_balance <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/eu_balance.csv")
eu_balance <- eu_balance %>% arrange((value))

budget_labels <- c(budget = "Budget Balance", 
                   current = "Current-account Balance") #creating plot labels

eu_balance %>% 
  mutate(
    colourin = case_when(
      country == "Germany" ~ "#003399",
      country == "Greece" ~ "#FF2B4F",
      country == "Netherlands" ~ "#fcab27",
      country == "Spain" ~ "#3686d3",
      T ~ "darkgray"),
    value = value / 1000 #scaling
    ) %>%
  ggplot(aes(year, value, fill = colourin)) + 
  geom_col() + 
  scale_fill_identity("Country", #name of legend
                      guide = "legend", #adding legend
                      labels = c("Germany", "Greece", "Netherlands", "Spain", "Other")) + #legend labels
  facet_grid(~ account_type, labeller= labeller(account_type = budget_labels)) +
  labs(title = "Surfeit of surpluses", 
       subtitle = "Euro-Area, bn Euros",
       x = "",  y = "",
       caption = "Source: Elsevier") 

#not 100% happy with color fill order

### Research Parity ####

women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

women_research <- women_research %>% spread(field, percent_women) %>% #into long form
  mutate(`Other STEMs` = (Engineering + `Physical sciences` + `Computer science, maths`)/3) %>% #get stem av
  select(country, 'Health sciences', `Other STEMs`) %>% #remove extra columns
  gather(field, percent_women, -country) #back into long form

ggplot(women_research, aes(fct_reorder(country, percent_women), percent_women)) + 
  geom_hline(yintercept= 0.5, alpha = 0.75, color = "red") + #parity line
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1)) + #changing scales highlights low numbers
  facet_grid(~ field) +
  labs(title = "Still a man's world", 
       subtitle = "Women among researches with papers published, 2011-2015",
       x = "",  y = "",
       caption = "Source: Elsevier") 

