# using drake for data provenance

# https://data.nozav.org/post/2019-a-short-introduction-to-drake/

library(drake)
library(dplyr)
library(ggplot2)


#  Get Examples -----------------------------------------------------------

drake_examples()

# Get an example
drake_example("mlr-slurm")
drake_example("slurm")
drake_example("code_to_plan")
drake_example("script-based-workflows")

# Load some Functions -----------------------------------------------------

create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram()
}


# Get the files with drake_example("main").
file.exists("raw_data.xlsx")
#> [1] TRUE
file.exists("report.Rmd")
#> [1] TRUE


## UPDATE
plan <- drake_plan(
  #raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
  raw_data = iris,
  data = raw_data %>%
    mutate(Species = forcats::fct_inorder(Species)),
  hist = create_plot(data),
  fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)
plan

# update
make(plan)


vis_drake_graph(plan)

clean()
