#!/usr/bin/env Rscript
library(rspecan)

specdb <- "spectra_db"
prospect_version <- c(4, 5, "5B", "D")
overwrite_all <- FALSE

prepare_inversion(
  specdb = specdb,
  prospect_version = prospect_version,
  overwrite_all = overwrite_all
)
