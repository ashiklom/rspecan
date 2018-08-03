#' @importFrom assertthat %has_name% %has_attr%
NULL

#' Check that metadata format is correct
#'
#' @inheritParams check_project 
#' @param metadata data.frame with metadata
#' @return 
#' @author Alexey Shiklomanov
check_metadata <- function(metadata, spectra_db) {

  metadata_yaml_check_file <- file.path(spectra_db, "metadata_header.csv")
  metadata_columns_check_file <- file.path(spectra_db, "metadata_header.csv")
  trait_check_file <- file.path(spectra_db, "traits.csv")

  metadata_yaml_check <- readr::read_csv(
    metadata_yaml_check_file,
    col_types = readr::cols(.default = "character")
  )
  metadata_columns_check <- readr::read_csv(
    metadata_columns_check_file,
    col_types = readr::cols(.default = "character")
  )
  trait_check <- readr::read_csv(
    trait_check_file,
    col_types = readr::cols(
      min = "numeric",
      max = "numeric",
      .default = "character"
    )
  )

  metadata_sub <- metadata %>%
    suppressWarnings(
      dplyr::select(dplyr::one_of(metadata_columns_check[["column"]]))
    )

  required_cols <- metadata_columns_check %>%
    dplyr::filter(required) %>%
    dplyr::pull(column)

  purrr::walk(
    required_cols,
    ~assertthat::assert_that(. %in% colnames(metadata_sub))
  )

  metadata_traits <- metadata %>%
    suppressWarnings(dplyr::select(dplyr::one_of(trait_check[["trait"]])))

  trait_units <- metadata_traits %>%
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
