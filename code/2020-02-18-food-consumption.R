# 2020-02-18

# https://github.com/rfordatascience/tidytuesday

# code from here:
# https://juliasilge.com/blog/food-hyperparameter-tune/

# Get Data ----------------------------------------------------------------

library(tidyverse)

food_consumption <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv")

## update

library(countrycode)
library(janitor)

food <- food_consumption %>%
  select(-co2_emmission) %>%
  pivot_wider(
    names_from = food_category,
    values_from = consumption
  ) %>%
  clean_names() %>%
  mutate(continent = countrycode(
    country,
    origin = "country.name",
    destination = "continent"
  )) %>%
  mutate(asia = case_when(
    continent == "Asia" ~ "Asia",
    TRUE ~ "Other"
  )) %>%
  select(-country, -continent) %>%
  mutate_if(is.character, factor)


# Visualize ---------------------------------------------------------------


library(GGally)
ggscatmat(food, columns = 1:11, color = "asia", alpha = 0.7)




# Bootstrapping -----------------------------------------------------------

library(tidymodels)

set.seed(1234)
food_boot <- bootstraps(food, times = 30)
food_boot

# model specs
rf_spec <- rand_forest(
  mode = "classification",
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_engine("ranger")

rf_spec



# Tuning Hyperparameters --------------------------------------------------

doParallel::registerDoParallel()

# look at all possible combos for model
rf_grid <- tune_grid(rf_spec, 
                     asia ~ .,
                     resamples = food_boot
                     )

rf_grid

# look at results
rf_grid %>%
  collect_metrics()

# look at best for specific result
rf_grid %>%
  show_best("roc_auc")
