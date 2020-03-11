
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
