#' Retrieve all metadata for given list of projects
#'
#' Currently, all metadata is stored in one large list in the object root
#'
#' @param projects Character vector of projects for which to load metadata. If 
#' `NULL` (default), load all projects.
#' @param include_metadata Logical. If `TRUE`, include a list column of metadata
#' @param unfactor Logical. If `TRUE`, convert factor columns to character
#' @param metadata_cols Vector of metadata items to extract as columns
#' @inheritParams load_spectra
#' @return Metadata for specified projects, as `tibble`
#' @export
get_metadata <- function(specdb, projects = NULL,
                         include_metadata = TRUE,
                         unfactor = TRUE,
                         metadata_cols = c("project_code", "short_name")) {
  if (is.null(projects)) {
    projects <- fs::dir_ls(specdb, type = "dir") %>% fs::path_file() %>% as.character()
  }

  all_df <- tibble::tibble(project_code = projects) %>%
    dplyr::mutate(data = purrr::map(
      project_code,
      get_project_metadata,
      specdb = specdb,
      unfactor = unfactor,
      date_as_posix = TRUE
      )
    )

  out <- tidyr::unnest(all_df, data)

  if (include_metadata) {
    meta <- purrr::map(all_df$data, metar::get_all_metadata) %>%
      setNames(projects)
    out <- metar::add_metadata(out, !!!meta)
  }
  out
}

#' Retrieve metadata for an individual project
#'
#' @inheritParams get_metadata
#' @param project String containing project name
#' @param date_as_posix Convert `Date` columns to `POSIXct`
#' @param pb Progress bar object (default = `NULL`)
#' @export
get_project_metadata <- function(specdb, project,
                                 unfactor = FALSE,
                                 date_as_posix = FALSE,
                                 metadata_cols = c("project_code", "short_name"),
                                 pb = NULL) {
  if (!is.null(pb)) pb$tick()
  traits <- yaml::read_yaml(fs::path(specdb, "traits.yml"))
  proj_path <- fs::path(specdb, project, "metadata.csvy")
  meta_raw <- metar::read_csvy(proj_path)
  if (unfactor) {
    meta_raw <- meta_raw %>%
      dplyr::mutate_if(is.factor, as.character)
  }
  if (date_as_posix) {
    meta_raw <- meta_raw %>%
      dplyr::mutate_if(~inherits(., "Date"), as.POSIXct)
  }
  if (length(metadata_cols) > 0) {
    meta_cols <- metar::metadata(meta_raw)[metadata_cols]
    meta_raw <- meta_raw %>%
      tibble::add_column(!!!meta_cols)
  }
  hastraits <- purrr::map_chr(traits, "trait") %>%
    intersect(colnames(meta_raw))
  if (length(hastraits) > 0) {
    data_units <- meta_raw[hastraits] %>%
      purrr::map(metar::metadata) %>%
      purrr::map_chr("data_unit")
    correct_units <- traits %>%
      purrr::keep(~.$trait %in% hastraits) %>%
      purrr::map_chr("unit")
    names(data_units) <- names(correct_units) <- hastraits
    for (trait in hastraits) {
      meta_raw[[trait]] <- udunits2::ud.convert(
        meta_raw[[trait]],
        data_units[trait],
        correct_units[trait]
      ) %>% metar::add_metadata(unit = correct_units[trait])
    }
  }
  meta_raw
}
