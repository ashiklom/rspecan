#' @importFrom assertthat %has_name% %has_attr%
check_metadata <- function(metadata) {
  assertthat::assert_that(
    metadata %has_name% "observation_id",
    metadata %has_name% "species_code",
    metadata %has_name% "year",
    metadata %has_name% "instrument_code",
    metadata %has_name% "latitude",
    metadata %has_name% "longitude",
    metadata %has_name% "is_experiment",
    metadata %has_attr% "project_code",
    metadata %has_attr% "short_name",
    metadata %has_attr% "long_name",
    metadata %has_attr% "spectra_methods"
  )

  trait_units <- metadata %>%
    dplyr::select(
      dplyr::starts_with("leaf_"),
      -dplyr::matches("leaf_age|leaf_temperature")
    ) %>%
    purrr::map(metar::metadata) %>%
    purrr::map_chr("data_unit", .default = NA_character_)

  assertthat::assert_that(
    all(!is.na(trait_units)),
    all(purrr::map_lgl(trait_units, udunits2::ud.is.parseable))
  )

  with(
    metadata,
    assertthat::assert_that(
      is.character(observation_id),
      !any(duplicated(observation_id)),
      is.character(species_code),
      is.numeric(year),
      is.character(instrument_code) || is.factor(instrument_code),
      is.numeric(latitude),
      is.numeric(longitude),
      is.logical(is_experiment)
    )
  )
  invisible(metadata)
}

check_spectra <- function(spectra) {
  assertthat::assert_that(
    tibble::is_tibble(spectra),
    nrow(spectra) == nrow(dplyr::distinct(spectra, spectra_id, wavelength))
  )
  invisible(spectra)
}
