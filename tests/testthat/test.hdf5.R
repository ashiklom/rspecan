library(rspecan)
library(testthat)

data(testspec, package = "PEcAnRTM")
dn1 <- "test1"
sp1 <- spectra(testspec_ACRU[, 1:5], 400:2500)
dn2 <- "test2"
sp2 <- spectra(testspec_ACRU[-(1902:2101), 6:10], 400:2300)

spectra_file <- tempfile()
test_that(
  "Writing spectra, with and without overwrite",
  {
    expect_silent(write_spectra(sp1, dn1, spectra_file))
    expect_silent(write_spectra(sp2, dn2, spectra_file))
    expect_error(write_spectra(sp1, dn1, spectra_file))
    expect_silent(write_spectra(sp1, dn1, spectra_file, overwrite = TRUE))
  }
)

sp_both <- read_spectra(spectra_file, c(dn1, dn2))
sp_all <- read_spectra(spectra_file, dn1)
sp_wl <- read_spectra(spectra_file, dn1, wavelength = 400:500)
ids <- colnames(testspec_ACRU)[c(1, 3, 5)]
sp_id <- read_spectra(spectra_file, dn1, spectra_id = ids)
suppressWarnings(.z <- file.remove(spectra_file))
