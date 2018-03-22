#!/usr/bin/env Rscript
library(rspecan)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- 1
}

args <- as.numeric(args)

queue_table <- "invert_queue"

stopifnot(
  all(is.numeric(args)),
  all(!is.na(args)),
  file.exists(queue_table)
)

queue <- read_inversion_table(inv_table, args)
purrr::pwalk(queue, run_inversion)
