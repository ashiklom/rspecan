create_project <- function(specdb_file, project_code, project_info, overwrite = FALSE) {
  check_project_info(project_info)
  specdb <- hdf5r::H5File$new(specdb_file)
  on.exit(spcdb$close_all)
  project <- h5_group(specdb, project_code, overwrite)
  project <- list2hdf(project, list(info = project_info), overwrite)

  spec_data <- h5_group(project, "spectra", overwrite)
  spec_data_list <- store_spectra(spec_data, spectra)

  metadata <- h5_group(project, "metadata", overwrite)
}

check_project_info <- function(project_info) {
  required_names <- c(
    "short_name",
    "long_name"
  )
  stopifnot(
    all(required_names %in% names(project_info))
  )
  invisible(project_info)
}
