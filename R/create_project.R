#' Create a project in the spectra database
#'
#' @param specdb_file Path to spectra file
#' @param project_code Character string of project code
#' @param project_info List of common project information
#' @param spectra Spectra object
#' @export
create_project <- function(specdb_file,
                           project_code,
                           project_info,
                           spectra,
                           metadata,
                           overwrite = FALSE) {
  check_project_info(project_info)
  check_spectra(spectra)
  check_metadata(metadata)

  assertthat::assert_that(
    all(colnames(spectra) %in% metadata$observation_id)
  )

  specdb <- hdf5r::H5File$new(specdb_file)
  on.exit(specdb$close_all())
  
  input_list <- list(
    info = project_info,
    spectra = spectra2list(spectra, colnames(spectra)),
    metadata = metadata
  )

  project <- h5_group(specdb, project_code, overwrite) %>%
    list2hdf(input_list, overwrite)
  invisible(TRUE)
}
