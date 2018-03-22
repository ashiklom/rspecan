library(rspecan)

data(testspec)
spectra <- spectra(testspec_ACRU, 400:2500)

specdb_file <- "test.h5"
overwrite <- TRUE
#file.remove(specdb_file)

project_code <- "test"

project_info <- list(
  short_name = "test",
  long_name = "Test spectra from PEcAnRTM package"
)
