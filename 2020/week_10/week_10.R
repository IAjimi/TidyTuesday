## Loading :ibraries
library(tidyverse)
library(leaps)
library(scales)

## Reading Data
tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 
salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')
historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')
diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

tuition_cost %>% head()
tuition_income %>% head()
salary_potential %>% head()
diversity_school %>% head()

## Do better ranked school deliver high early and mid career returns (early_career / total_price)?
salary_potential %>%
  left_join(select(tuition_income, name, total_price, year)) %>%
  filter(!is.na(total_price)) %>%
  filter(year == max(year)) %>%
  mutate(roi = early_career_pay / total_price)

# RETURNS TO EDUCATION: PUBLIC V. PRIVATE ####
salary_label <- c(early_career_pay = "Early Career", mid_career_pay = "Mid Career")

salary_potential_by_type <- salary_potential %>%
  left_join(select(tuition_income, name, total_price, year)) %>%
  left_join(select(tuition_cost, name, state_name = state, type, degree_length)) %>%
  filter(!is.na(total_price), !is.na(type)) %>%
  group_by(name, type) %>%
  summarise(early_career_pay = mean(early_career_pay, na.rm = TRUE),
            mid_career_pay = mean(mid_career_pay, na.rm = TRUE),
            total_price = mean(total_price, na.rm = TRUE)) %>%
  rename(`Early Career` = early_career_pay, `Mid Career` = mid_career_pay) %>%
  gather(pay_key, pay_value, c(`Early Career`, `Mid Career`))

salary_potential_by_type %>%
  ggplot(aes(total_price, pay_value, color = type)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values=c("#2C5A85", "#884147")) +
  scale_x_continuous(breaks = seq(0, 70000, 12500), trans = "log", labels = dollar) +
  scale_y_continuous(breaks = seq(0, 170000, 25000), trans = "log", labels = dollar) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Yearly Tuition Cost", y = "Salary", color = "Type",
       title = "Overall Returns-to-Education Across Private and Public Colleges",
       subtitle = "But Highest Salaries Remain Province of Private Colleges") +
  facet_grid(~ pay_key)


# COLLEGE "ALPHA": EXPECTED V. ACTUAL EARLY CAREER PAY ####
## Prepping Data
white_div <- diversity_school %>%
  filter(category == "White") %>%
  mutate(pct_white = enrollment / total_enrollment) %>%
  select(name, total_enrollment, state_name = state, pct_white)

women_div <- diversity_school %>%
  filter(category == "Women") %>%
  mutate(pct_women = enrollment / total_enrollment) %>%
  select(name, total_enrollment, state_name = state, pct_women)

## Creating Dataset
educational_returns <- salary_potential %>%
  left_join(select(tuition_income, name, total_price, year)) %>%
  left_join(select(tuition_cost, name, state_name = state, type, degree_length)) %>%
  left_join(white_div) %>%
  left_join(women_div) %>%
  filter(!is.na(total_price)) %>%
  distinct()

# Finding Best Predictive Model, excluding variables with high missingness
models <- regsubsets(early_career_pay ~ rank + state_name + stem_percent + total_price + year + type + degree_length  + 
                       total_enrollment +  pct_white + pct_women, 
                     data = educational_returns, nvmax = 20, method = "seqrep")
summary(models)

## Keeping Most Relevant States 
educational_returns <- educational_returns %>%
  mutate(sign_state = case_when(
    state_name %in% c("Arkansas", "California", "Connecticut", "Illinois", , 
                      "Kentucky", "Maryland", "Massachusetts", "Mississippi", "Pennsylvania",
                      "Texas", "Virginia", "Washington") ~ state_name,
    T ~ "Other"
  )
  )

reg <- lm(early_career_pay ~ rank + state_name + stem_percent + total_price + 
            total_enrollment +  pct_white, 
          data = educational_returns)
summary(reg)

## Adding Residuals to Most Recent Data 
educational_returns_2018 <- educational_returns %>% filter(year >= 2015, !is.na(total_enrollment))

educational_returns_2018$pred_early_career_pay <- predict(reg, newdata = educational_returns_2018)

edu_plot <- educational_returns_2018 %>% 
  mutate(school_performance = early_career_pay - pred_early_career_pay) %>% 
  filter(!is.na(early_career_pay))

edu_plot %>%
  group_by(name) %>%
  summarise(school_performance = mean(school_performance, na.rm = TRUE)) %>%
  filter(school_performance > quantile(.$school_performance, 0.99, na.rm = TRUE) | school_performance < quantile(.$school_performance, 0.01, na.rm = TRUE)) %>%
  ggplot(aes(fct_reorder(name, school_performance), school_performance)) +
  geom_col() + 
  coord_flip() +
  scale_y_continuous(limits = c(-12500, 12500), labels = dollar) +
  labs(title = "School Performance", 
       subtitle = "Difference between Actual and Expected Early Career Pay",
       x = "",  y = "") 

