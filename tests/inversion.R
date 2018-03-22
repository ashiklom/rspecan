library(rspecan)

project <- "lopex"
observation_id <- "lopex_50"
prospect_version <- 5
specdb <- "spectra_db"
spectra_types <- c("R", "CRR", "PA")
overwrite <- TRUE

inv_table <- get_status_table(specdb, prospect_version)

run_inversion(
  project,
  observation_id,
  specdb,
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
