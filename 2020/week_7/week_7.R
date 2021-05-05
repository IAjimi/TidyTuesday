# TIDYTUESDAY 2020 WEEK 7
# Exploring tidymodels with https://www.tidymodels.org/start/case-study/
# Predicting hotel cancellations & no-shows

#### SETUP ####
library(tidyverse)
library(tidymodels)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')
hotels <- hotels %>% 
  select(-is_canceled, - reservation_status_date, -country, -agent, -company) %>%
  mutate(reservation_status = if_else(reservation_status %in% c('No-Show', 'Canceled'), '1', '0'),
         reservation_status = as.factor(reservation_status),
         children = replace_na(children, 0)
         )


#### EDA ####
hotels %>% summary()

hotels %>% 
  count(hotel) %>% 
  mutate(prop = n/sum(n))

hotels %>% 
  count(meal) %>% 
  mutate(prop = n/sum(n))

hotels %>% # maybe regroup to continent?
  count(country) %>% 
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop))

hotels %>% 
  count(market_segment) %>% 
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop))

hotels %>% 
  count(company) %>% # mostly NULL
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop))

#### MODELS ####
set.seed(15)
splits <- initial_split(hotels, strata = reservation_status)
other <- training(splits)
validation_set <- validation_split(
                            other, 
                            strata = reservation_status, 
                            prop = 0.8)
test  <- testing(splits)


### Log Reg w/ penalization
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(reservation_status ~ ., data = other) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% # converts char. & factors to dummies
  step_zv(all_predictors()) %>% # removes cols w/ unique value
  step_normalize(all_predictors()) # normalizes numeric predictors

lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30)) # only 1 tuning param for this

lr_res <- 
  lr_workflow %>% 
  tune_grid(validation_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

lr_res$.notes[[1]][[1]]

lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 
# better model performance at smaller penalty values
#  majority of the predictors are important to the model
# will pick model w best perf and highest penalty

lr_best <- lr_res %>% 
  collect_metrics() %>% 
  filter(round(mean, 3) == round(max(mean), 3)) %>% 
  arrange(penalty) %>% 
  tail(1)

lr_best # 0.913 roc_auc, 0.000530 penalty

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(reservation_status, .pred_0) %>% # likelihood of not cancelled
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

### On test
glmn_fit <- 
  logistic_reg(penalty = 0.000530, mixture = 1) %>% 
  set_engine("glmnet")

tuned_glmn_workflow <- 
  lr_workflow %>% 
  update_model(glmn_fit)

last_fit <- 
  tuned_glmn_workflow %>% 
  last_fit(splits)

last_fit %>% 
  collect_metrics() # 83% accuracy, 0.914 ROC AUC on test

last_fit %>% 
  collect_predictions() %>% 
  roc_curve(reservation_status, .pred_0) %>% 
  autoplot()
