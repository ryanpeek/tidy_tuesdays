
# 2020-MAR-10 -------------------------------------------------------------

# https://github.com/rfordatascience/tidytuesday
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-10/readme.md

# see here: https://juliasilge.com/blog/tuition-resampling/


# Load Data ---------------------------------------------------------------

library(tidyverse)

tuition_cost <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv")

diversity_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv") %>%
  filter(category == "Total Minority") %>%
  mutate(TotalMinority = enrollment / total_enrollment)


# Take a Look -------------------------------------------------------------

# look at distribution
diversity_school <- diversity_raw %>%
  filter(category == "Total Minority") %>%
  mutate(TotalMinority = enrollment / total_enrollment)

diversity_school %>%
  ggplot(aes(TotalMinority)) +
  geom_histogram(alpha = 0.7, fill = "midnightblue") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "% of student population who identifies as any minority")

# build modeling dataset and join with state/regions
university_df <- diversity_school %>%
  filter(category == "Total Minority") %>%
  mutate(TotalMinority = enrollment / total_enrollment) %>%
  transmute(
    diversity = case_when(
      TotalMinority > 0.3 ~ "high",
      TRUE ~ "low"
    ),
    name, state,
    total_enrollment
  ) %>%
  inner_join(tuition_cost %>%
               select(
                 name, type, degree_length,
                 in_state_tuition:out_of_state_total
               )) %>%
  left_join(tibble(state = state.name, region = state.region)) %>%
  select(-state, -name) %>%
  mutate_if(is.character, factor)

skimr::skim(university_df)

# look at quantities in relation to proportion of minority students at a college?

university_df %>%
  ggplot(aes(type, in_state_tuition, fill = diversity)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = NULL, y = "In-State Tuition", fill = "Diversity")


# Set Up Model ------------------------------------------------------------

#devtools::install_github("tidymodels/tune")
library(tune)
library(tidymodels)

set.seed(1234)

# split data
table(university_df$diversity) # see what strata to use
uni_split <- initial_split(university_df, strata = diversity)
uni_train <- training(uni_split) # make a training set 
uni_test <- testing(uni_split) # make a testing set

# make the "recipe": 
uni_rec <- recipe(diversity ~ ., data = uni_train) %>%
  # filter out highly correlated
  step_corr(all_numeric()) %>% 
  # convert factor cols to binary dummy vars
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  # remove any numeric variables that have zero variance
  step_zv(all_numeric()) %>%
  # normalize (center and scale) the numeric variables
  step_normalize(all_numeric()) %>%
  # does the steps above
  prep()

uni_rec

# Run Logistic Regression -------------------------------------------------

# squeeze the recipe with training data so it's transformed
# basically create the final modeling dataset
uni_juiced <- juice(uni_rec)

# set up a logistic regression
glm_spec <- logistic_reg() %>%
  set_engine("glm")

# fit logistic regression
glm_fit <- glm_spec %>%
  fit(diversity ~ ., data = uni_juiced)

# view
glm_fit

# Run K-Nearest Neighbor --------------------------------------------------

knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_fit <- knn_spec %>%
  fit(diversity ~ ., data = uni_juiced)

knn_fit

# Run RPart Decision Tree ------------------------------------------------

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_fit <- tree_spec %>%
  fit(diversity ~ ., data = uni_juiced)

tree_fit

# Evaluate and Resample ---------------------------------------------------

set.seed(123)
folds <- vfold_cv(uni_juiced, strata = diversity)

set.seed(234)

glm_rs <- glm_spec %>% 
  fit_resamples(diversity ~ .,
                folds,
                metrics = metric_set(roc_auc, sens, spec),
                control = control_resamples(save_pred = TRUE)
  )

# collect the results
glm_rs %>%
  collect_metrics()

set.seed(234)
knn_rs <- knn_spec %>%
  fit_resamples(diversity ~ .,
                folds,
                metrics = metric_set(roc_auc, sens, spec),
                control = control_resamples(save_pred = TRUE)
  )

knn_rs %>%
  collect_metrics()

set.seed(234)
tree_rs <- tree_spec %>%
  fit_resamples(diversity ~ .,
                folds,
                metrics = metric_set(roc_auc, sens, spec),
                control = control_resamples(save_pred = TRUE)
  )

tree_rs %>%
  collect_metrics()

# check it out!
tree_rs


# Visualize ---------------------------------------------------------------

glm_rs %>%
  unnest(.predictions) %>%
  mutate(model = "glm") %>%
  bind_rows(knn_rs %>%
              unnest(.predictions) %>%
              mutate(model = "knn")) %>%
  bind_rows(tree_rs %>%
              unnest(.predictions) %>%
              mutate(model = "rpart")) %>%
  group_by(model) %>%
  roc_curve(diversity, .pred_high) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(size = 1.5) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )


# arrange by estimate
glm_fit %>%
  tidy() %>%
  arrange(-estimate)


# Predict -----------------------------------------------------------------

# Use test data to do unbiased check on how we can expect this model to perform on new data.
# bake() our recipe using the testing set to apply the same preprocessing steps that we used on the training data

glm_fit %>%
  predict(
    new_data = bake(uni_rec, uni_test),
    type = "prob"
  ) %>%
  mutate(truth = uni_test$diversity) %>%
  roc_auc(truth, .pred_high)  

# check specificity
glm_fit %>%
  predict(
    new_data = bake(uni_rec, new_data = uni_test),
    type = "class"
  ) %>%
  mutate(truth = uni_test$diversity) %>%
  spec(truth, .pred_class)
