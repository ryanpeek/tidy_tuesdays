# get tidy tuesday data:
# devtools::install_github("thebioengineer/tidytuesdayR")


# test
tuesdata <- tidytuesdayR::tt_load('2020-01-28') 
tuesdata <- tidytuesdayR::tt_load(2020, week = 5)