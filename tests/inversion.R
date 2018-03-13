library(rspecan)

project <- "lopex"
observation_id <- "lopex_50"
prospect_version <- 5
specdb_file <- "test.h5"
spectra_types <- c("R", "CRR", "PA")
overwrite <- FALSE

run_inversion(
  project,
  observation_id,
  specdb_file,
  prospect_version,
  overwrite = TRUE
)

#m1 <- matrix(1:20, 4)
#m2 <- matrix(1:8, 4)

#mod <- observed[,1]
#m <- as.numeric(mod)

#mb <- microbenchmark::microbenchmark(
  #sweep_mod = sweep(-observed, 1, mod, "+"),
  #sweep_vec = sweep(-observed, 1, m, "+"),
  #arith_vec = -observed + m,
  #times = 1000
#)
