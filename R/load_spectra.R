#' Load single set of observed spectra from HDF5 file
#'
#' @param project_code Character string of project code from which to load spectra
#' @param observation_id Observation ID of spectra to load
#' @param specdb Path to spectra database
#' @param wl_min Minimum wavelength to retrieve, or `NULL` (default) for no minimum
#' @param wl_max Maximum wavelength to retrieve, or `NULL` (default) for no maximum
#' @param wavelengths Wavelengths to which to [PEcAnRTM::resample] results, or 
#' `NULL` (default) to retrieve as is
#' @param spectra_types Spectra types to retrieve, or `NULL` (default) for all 
#' types. See [PEcAnRTM::valid_spectra_types].
#' @return [PEcAnRTM::spectra] object
#' @export
load_spectra <- function(project_code,
                         observation_id,
                         specdb,
                         wl_min = NULL,
                         wl_max = NULL,
                         wavelengths = NULL,
                         spectra_types = NULL) {

  assertthat::assert_that(
    assertthat::is.string(project_code),
    assertthat::is.string(observation_id)
  )

  spectra_path <- fs::path(specdb, project_code, "spectra", observation_id, ext = "csvy")
  stopifnot(fs::file_exists(spectra_path))

  spec <- read_spectra(spectra_path)

  waves <- PEcAnRTM::wavelengths(spec)
  lwaves <- rep(TRUE, length(waves))
  if (!is.null(wl_min)) {
    assertthat::assert_that(
      assertthat::is.number(wl_min)
    )
    lwaves <- lwaves & waves >= wl_min
  }
  if (!is.null(wl_max)) {
    assertthat::assert_that(
      assertthat::is.number(wl_max)
    )
    lwaves <- lwaves & waves <= wl_max
  }
  iwaves <- which(lwaves)
  assertthat::assert_that(length(iwaves) > 0)

  types <- PEcAnRTM::spectra_types(spec)
  if (!is.null(spectra_types)) {
    assertthat::assert_that(is.character(spectra_types))
    itypes <- which(types %in% spectra_types)
  } else {
    itypes <- seq_along(types)
  }

  assertthat::assert_that(length(itypes) > 0)

  spec <- spec[iwaves, itypes]

  if (!is.null(wavelengths)) {
    spec <- PEcAnRTM::resample(spec, wavelengths)
  }

  spec
}

#' Read spectra from CSVY file
#'
#' @param filename Target file to read
#' @param ... Additional arguments to [metar::read_csvy]
#' @return Object of class `spectra`
#' @export
read_spectra <- function(filename, ...) {
  spec_tbl <- metar::read_csvy(filename)
  spec_mat <- spec_tbl %>%
    dplyr::select(-wavelengths) %>%
    as.matrix()
  waves <- spec_tbl[["wavelengths"]]
  spectypes <- attr(spec_tbl, "spectra_types")
  PEcAnRTM::spectra(spec_mat, waves, spectypes)
}

#' Add spectra column to metadata
#'
#' @param data `data.frame` to which to add spectra. Must include columns 
#' `project_code` and `observation_id`
#' @inheritParams load_spectra
#' @param ... Additional arguments to [load_spectra]
#' @return `data`, with `spectra` as a list column of [PEcAnRTM::spectra] objects.
#' @export
add_spectra_column <- function(data, specdb, ...) {
  assertthat::assert_that(
    is.data.frame(data),
    data %has_name% "project_code",
    data %has_name% "observation_id"
  )
  data %>%
    dplyr::mutate(
      spectra = purrr::map2(
        project_code,
        observation_id,
        load_spectra,
        specdb = specdb, ...)
    )
}

#' Convert spectra list column to single spectra object
#'
#' @param data `data.frame` from which to extract spectra.
#' @param spectra_col Column name (quoted or unquoted, as in [dplyr::pull]) containing spectra
#' @return Single [PEcAnRTM::spectra] object containing all spectra in `data`
#' @export
pull_spectra <- function(data, spectra_col = "spectra") {
  data %>%
    dplyr::pull(!!spectra_col) %>%
    do.call(cbind, .)
}
