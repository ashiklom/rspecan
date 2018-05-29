library(drake)

source("R-drake/clean_results.R")
source("R-drake/prospect_pairs_plots.R")
source("R-drake/plan.R")

config <- drake_config(my_plan)
check_plan(my_plan)
make(my_plan)
