#' Convert a `spectra` object into a long `tibble`
#'
#' Also, create a unique `spectra_id` column, and change the column
#' names to match the specification in the README.
#'
#' @param spectra Object of class `spectra`
#' @return Long `tibble` with each wavelength and observation in one row
#' @export
make_long_spectra <- function(spectra) {
  PEcAnRTM::tidy_spectra(spectra) %>%
    dplyr::transmute(
      observation_id = col_name,
      spectra_id = paste(observation_id, spectra_type, col_rep, sep = "__"),
      spectra_type = spectra_type,
      wavelength = wavelength,
      value = value
    )
}
