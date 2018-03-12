check_spectra <- function(spectra) {
  assertthat::assert_that(
    is_spectra(spectra)
  )
  invisible(spectra)
}
