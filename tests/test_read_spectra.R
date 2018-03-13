library(rspecan)

specdb <- "test.h5"
project <- "lopex"
observation_id <- "lopex_55"

s1 <- load_spectra(project, observation_id, specdb, spectra_types = "R")

m <- get_metadata(specdb)

s <- m %>%
  filter(leaf_chla_per_area < 2e-4) %>%
  add_spectra_column(specdb_file, wl_max = 1500) %>%
  pull_spectra()
