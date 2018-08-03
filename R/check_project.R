#' Check the validity of a spectra database project
#'
#' @param project_name Name of the project to check, as character
#' @param spectra_db Path to the spectra database. Default = "spectra_db"
#' @return `TRUE` if successful, or throw an error if anything is wrong.
#' @author Alexey Shiklomanov
#' @export
check_project <- function(project_name, spectra_db = "spectra_db") {
  project_path <- file.path(spectra_db, project_name)

  spectra_check_file <- file.path(spectra_db, "spectra_format.csv")
  
  metadata_file <- file.path(project_path, "metadata.csvy")

  spectra_file <- file.path(project_path, "spectra.fst")

  stopifnot(
    dir.exists(project_path),
    file.exists(metadata_file),
    file.exists(spectra_file)
  )

  metadata <- metar::read_csvy(metadata_file)
  check_metadata(metadata, spectra_db)

  spectra <- fst::read_fst(spectra_file)
  check_spectra(spectra, spectra_db)

  return(TRUE)
}
