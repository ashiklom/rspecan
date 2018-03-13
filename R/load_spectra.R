#' Load single set of observed spectra from HDF5 file
#'
#' @param project Character string of project code from which to load spectra
#' @param observation_id Observation ID of spectra to load
#' @param specdb H5File object containing spectra database, or character path 
#' to containing file
#' @param wl_min Minimum wavelength to retrieve, or `NULL` (default) for no minimum
#' @param wl_max Maximum wavelength to retrieve, or `NULL` (default) for no maximum
#' @param wavelengths Wavelengths to which to [PEcAnRTM::resample] results, or 
#' `NULL` (default) to retrieve as is
#' @param spectra_types Spectra types to retrieve, or `NULL` (default) for all 
#' types. See [PEcAnRTM::valid_spectra_types].
#' @return [PEcAnRTM::spectra] object
#' @export
load_spectra <- function(project,
                         observation_id,
                         specdb,
                         wl_min = NULL,
                         wl_max = NULL,
                         wavelengths = NULL,
                         spectra_types = NULL) {

  assertthat::assert_that(
    assertthat::is.string(project),
    assertthat::is.string(observation_id)
  )

  if (is.character(specdb)) {
    specdb <- h5_open(specdb)
    on.exit(specdb$close_all())
  }

  h5_obs <- specdb[[h5_path(project, "spectra", observation_id)]]

  waves <- h5_obs[["wavelengths"]][]
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

  types <- h5_obs[["spectra_types"]][]
  if (!is.null(spectra_types)) {
    assertthat::assert_that(is.character(spectra_types))
    itypes <- which(types %in% spectra_types)
  } else {
    itypes <- seq_along(types)
  }

  result <- PEcAnRTM::spectra(
    h5_obs[["spectra"]][iwaves, itypes],
    wavelengths = waves[iwaves],
    spectra_types = types[itypes]
  )
  colnames(result) <- rep(observation_id, ncol(result))
  
  if (!is.null(wavelengths)) {
    result <- PEcAnRTM::resample(result, wavelengths)
  }

  result
}

#' Retrieve all metadata for given project
#'
#' @param projects Character vector of projects for which to load metadata. If 
#' `NULL` (default), load all projects.
#' @inheritParams load_spectra
#' @return Metadata for specified projects, as `tibble`
#' @export
get_metadata <- function(specdb, projects = NULL) {
  specdb <- as.H5File(specdb)
  if (is.null(projects)) {
    projects <- specdb$names
  }
  result <- tibble::tibble(project_code = projects) %>%
    dplyr::mutate(metadata = purrr::map(project_code, ~specdb[[.]][["metadata"]][])) %>%
    tidyr::unnest(metadata)
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
  specdb <- as.H5File(specdb)
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
        specdb = as.H5File(specdb), ...)
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
