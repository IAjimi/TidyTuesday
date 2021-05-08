## WEEK 2: TRANSIT COSTS
#### SETUP ####
library(tidyverse)
library(tidymodels)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_cost <- transit_cost %>%
  select(country, city, line, start_year, end_year, ppp_rate, length, stations, real_cost, rr) %>%
  mutate(real_cost = as.numeric(real_cost), # some are characters, not actual values
         start_year = as.numeric(start_year),
         end_year = as.numeric(end_year),
         duration = end_year - start_year
         ) %>%
  filter(!is.na(real_cost) & !is.na(start_year) & start_year < 2020) %>%
  select(-end_year) %>%
  drop_na()

transit_cost %>% summary()

#### MODELING #####
# Which projects were more expensive than expected?
set.seed(15)
transit_split <- initial_split(transit_cost, prop = 0.75, strata = real_cost) # strate ensures training outcome var is representative 
training <- training(transit_split)
testing <- testing(transit_split)

linear_model <- linear_reg(penalty = tune()) %>% set_engine('glmnet') %>% set_mode('regression')

transit_recipe <- recipe(real_cost ~ ., data = training) %>%
  update_role(country, city, line, new_role = "ID") %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

summary(transit_recipe)

transit_recipe %>% 
  prep(training = training) %>%
  bake(new_data = NULL) %>% tail()

transit_wkfl <- workflow() %>% 
  add_model(linear_model) %>% 
  add_recipe(transit_recipe)

lm_folds <- vfold_cv(training, v = 5, strata = real_cost)
lm_grid <- grid_random(parameters(linear_model), size = 15)

lm_tuning <- transit_wkfl %>%
  tune_grid(
    resamples = lm_folds, 
    grid = lm_grid, 
    metrics = metric_set(rmse, rsq))

lm_tuning %>% collect_metrics() # not enough variables for penalty tuning to matter

best_lm_model <- lm_tuning %>%  select_best(metric = 'rmse', n = 5) # penalty basically 0

transit_wkfl_fit <- transit_wkfl %>% 
  finalize_workflow(best_lm_model) %>% 
  last_fit(split = transit_split)

transit_wkfl_fit %>% collect_metrics(summarize=F)

#### FIT FINAL MODEL WITH 0 PENALTY ####
set.seed(15)
transit_split <- initial_split(transit_cost, prop = 0.75, strata = real_cost) # strate ensures training outcome var is representative 
training <- training(transit_split)
testing <- testing(transit_split)

linear_model <- linear_reg(penalty = 0) %>% set_engine('glmnet') %>% set_mode('regression')

transit_recipe <- recipe(real_cost ~ ., data = training) %>%
  update_role(country, city, line, new_role = "ID") %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

transit_wkfl <- workflow() %>% 
  add_model(linear_model) %>% 
  add_recipe(transit_recipe)

transit_cost <- transit_wkfl %>% 
  fit(training) %>%
  predict(transit_cost) %>%
  cbind(transit_cost)

#### DATA VIZ ####
ggplot(transit_cost, aes(real_cost, .pred)) + 
  geom_abline(color = 'red', linetype = 2, alpha = 0.7) +
  geom_point(alpha = 0.3) +
  coord_obs_pred() +
  labs(x = 'Actual Project Cost', y = 'Predicted Project Cost',
       title = 'Transit Project Cost Prediction') +
  theme_bw()
# model is much more accurate for projects <= 10000 (bulk of the data is there)

ggplot(transit_cost, aes(real_cost, .pred, label = city)) + 
  geom_abline(color = 'red', linetype = 2, alpha = 0.7) +
  geom_point(alpha = 0.3) +
  geom_point(data = . %>% filter(city == 'New York'), 
             size = 1.6, alpha = 0.5, color = '#003399') +
  ggrepel::geom_text_repel(data = . %>% filter(line == 'Gateway'),
                           color = '#003399', 
                           nudge_x = -1500, 
                           nudge_y = -800) + # label Crusader Kings II
  #guides(color = F) +
  lims(x = c(0, 18000), y = c(0, 18000)) +
  labs(x = 'Actual Project Cost', y = 'Predicted Project Cost',
       title = 'NYC Transit Costs Much Higher Than Expected',
       subtitle = 'Even when controlling for project duration, number of stations, PPP, and tunnel length.') +
  theme_bw()

# Projects that were costlier than expected
transit_cost %>%
  select(country, city, line, duration, stations, length, real_cost, .pred) %>%
  mutate(.error = real_cost - .pred) %>%
  arrange(desc(.error)) %>%
  head(15) 

# all above average duration and stations and high length
# but duration and stations have mostly a linear relationship to cost

