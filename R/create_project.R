#' Create a project in the spectra database
#'
#' @param specdb Path to spectra database directory
#' @param project_code Character string of project code
#' @param spectra Spectra object
#' @export
create_project <- function(specdb,
                           project_code,
                           spectra,
                           metadata,
                           overwrite = FALSE) {
  spectra <- make_long_spectra(spectra)
  
  check_metadata(metadata)
  check_spectra(spectra)

  stopifnot(metar::metadata(metadata)$project_code == project_code)

  if (!fs::dir_exists(specdb)) {
    stop("Spectra database does not exist at ", specdb, ".")
  }

  fs::dir_create(specdb)

  proj_path <- fs::path(specdb, project_code)

  if (fs::dir_exists(proj_path) && !overwrite) {
    stop("Project already exists")
  }

  fs::dir_create(proj_path)

  assertthat::assert_that(
    all(unique(spectra$observation_id) %in% metadata$observation_id)
  )

  metar::write_csvy(metadata, filename = fs::path(proj_path, "metadata.csvy"))
  fst::write_fst(spectra, filename = fs::path(proj_path, "spectra.fst"))

  invisible(TRUE)
}
