# 2020-FEB-05 -------------------------------------------------------------

# https://github.com/rfordatascience/tidytuesday

# code from here:
# https://juliasilge.com/blog/intro-tidymodels/


# Get Data ----------------------------------------------------------------

library(tidyverse)

attendance <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv")
standings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv")

attendance_joined <- attendance %>%
  left_join(standings,
            by = c("year", "team_name", "team")
  )

attendance_joined


# Visualize ---------------------------------------------------------------

attendance_joined %>%
  filter(!is.na(weekly_attendance)) %>%
  ggplot(aes(fct_reorder(team_name, weekly_attendance),
             weekly_attendance,
             fill = playoffs
  )) +
  geom_boxplot(outlier.alpha = 0.5) +
  coord_flip() +
  labs(
    fill = NULL, x = NULL,
    y = "Weekly NFL game attendance"
  )

# how much does margin of victory matter in relation to playoffs?
attendance_joined %>%
  distinct(team_name, year, margin_of_victory, playoffs) %>%
  ggplot(aes(margin_of_victory, fill = playoffs)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  labs(
    x = "Margin of victory",
    y = "Number of teams",
    fill = NULL
  )

# changes with week of season? (more earlier or later)
attendance_joined %>%
  mutate(week = factor(week)) %>%
  ggplot(aes(week, weekly_attendance, fill = week)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.5) +
  labs(
    x = "Week of NFL season",
    y = "Weekly NFL game attendance"
  )


# clean data:
# remove the weeks that each team did not play 
# keep cols for modeling, keep margin_of_victory, strength_of_schedule, rm simple_rating

attendance_df <- attendance_joined %>%
  filter(!is.na(weekly_attendance)) %>%
  select(
    weekly_attendance, team_name, year, week,
    margin_of_victory, strength_of_schedule, playoffs
  )

attendance_df


# Load TidyModels ---------------------------------------------------------

library(tune)
library(tidymodels)

set.seed(1234)
attendance_split <- attendance_df %>%
  initial_split(strata = playoffs)

# split data
nfl_train <- training(attendance_split)
nfl_test <- testing(attendance_split)

# set up specifications for model
lm_spec <- linear_reg() %>%
  set_engine(engine = "lm")

lm_spec

# fit the model
lm_fit <- lm_spec %>%
  fit(weekly_attendance ~ .,
      data = nfl_train
  )

lm_fit

# make one more rf model
rf_spec <- rand_forest(mode = "regression") %>%
  set_engine("ranger")

# and fit
(rf_fit <- rf_spec %>%
  fit(weekly_attendance ~ .,
      data = nfl_train))


# Evaluate Models ---------------------------------------------------------

# Predict weekly attendance for both the training / testing
# Use both the OLS and random forest models

# training data
results_train <- lm_fit %>%
  predict(new_data = nfl_train) %>%
  mutate(
    truth = nfl_train$weekly_attendance,
    model = "lm"
  ) %>%
  bind_rows(rf_fit %>%
              predict(new_data = nfl_train) %>%
              mutate(
                truth = nfl_train$weekly_attendance,
                model = "rf"
              ))

# testing data
results_test <- lm_fit %>%
  predict(new_data = nfl_test) %>%
  mutate(
    truth = nfl_test$weekly_attendance,
    model = "lm"
  ) %>%
  bind_rows(rf_fit %>%
              predict(new_data = nfl_test) %>%
              mutate(
                truth = nfl_test$weekly_attendance,
                model = "rf"
              ))

# look at RMSE for train
results_train %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)


# look at RMSE for test
results_test %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

# visualize
results_test %>%
  mutate(train = "testing") %>%
  bind_rows(results_train %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred, color = model)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~train) +
  labs(
    x = "Truth",
    y = "Predicted attendance",
    color = "Type of model"
  )



# Cross Fold Validation ---------------------------------------------------

set.seed(1234)
nfl_folds <- vfold_cv(nfl_train, strata = playoffs)

rf_res <- fit_resamples(
  weekly_attendance ~ .,
  rf_spec,
  nfl_folds,
  control = control_resamples(save_pred = TRUE)
)

rf_res %>%
  collect_metrics()


# visualize
rf_res %>%
  unnest(.predictions) %>%
  ggplot(aes(weekly_attendance, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray20", size = 1.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(id~.) +
  labs(
    x = "Truth",
    y = "Predicted game attendance",
    color = NULL
  )
