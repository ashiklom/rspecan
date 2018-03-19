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
  check_metadata(metadata)
  check_spectra(spectra)

  stopifnot(metadata(metadata)$project_code == project_code)

  if (!fs::dir_exists(specdb)) {
    stop("Spectra database does not exist at ", specdb, ".")
  }

  fs::dir_create(specdb)

  proj_path <- fs::path(specdb, project_code)

  if (fs::dir_exists(proj_path) && !overwrite) {
    stop("Project already exists")
  }

  fs::dir_create(proj_path)
  spectra_path <- fs::path(proj_path, "spectra")
  fs::dir_create(spectra_path)

  assertthat::assert_that(
    all(colnames(spectra) %in% metadata$observation_id)
  )

  metar::write_csvy(metadata, filename = fs::path(proj_path, "metadata.csvy"))

  spectra_list <- spectra2list(spectra, colnames(spectra))
  spectra_names <- fs::path(spectra_path, names(spectra_list), ext = "csvy")

  walk2(spectra_list, spectra_names, metar::write_csvy)

  invisible(TRUE)
}
