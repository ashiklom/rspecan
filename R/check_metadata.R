check_metadata <- function(metadata) {
  assertthat::assert_that(
    metadata %has_name% "observation_id",
    metadata %has_name% "species_code",
    metadata %has_name% "year",
    metadata %has_name% "instrument_code",
    metadata %has_name% "latitude",
    metadata %has_name% "longitude",
    metadata %has_name% "is_experiment"
  )
  with(
    metadata,
    assertthat::assert_that(
      is.character(observation_id),
      !any(duplicated(observation_id)),
      is.character(species_code),
      is.numeric(year),
      is.character(instrument_code),
      is.numeric(latitude),
      is.numeric(longitude),
      is.logical(is_experiment)
    )
  )
  invisible(metadata)
}
