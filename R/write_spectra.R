#' Write spectra to HDF5 file
#'
#' @param spectra Spectra to store, either a matrix with wavelengths as row 
#' names, or an object of class `spectra`
#' @param data_name Data name for storing spectra
#' @param spectra_file File name for HDF5 file
#' @param overwrite Delete dataset `data_name` in HDF5 file before writing.
#' @return Spectra file name (invisibly)
#' @export
write_spectra <- function(spectra, data_name, spectra_file, overwrite = FALSE) {
  rn <- rownames(spectra)
  spectra_ids <- colnames(spectra)
  stopifnot(
    is_spectra(spectra) || !is.null(rn),
    !is.null(spectra_ids),
    !any(duplicated(spectra_ids))
  )
  if (is_spectra(spectra)) {
    wl <- wavelengths(spectra)
  } else if (!is.null(rn)) {
    wl <- as.numeric(rn)
  }
  hfile <- h5::h5file(spectra_file)
  if (overwrite) {
    h5::h5unlink(hfile, data_name)
  }
  data_spec <- paste0(data_name, "/spectra")
  data_wl <- paste0(data_name, "/wavelengths")
  data_id <- paste0(data_name, "/spectra_id")
  hfile[data_spec] <- spectra
  hfile[data_wl] <- wl
  hfile[data_id] <- spectra_ids
  h5::h5close(hfile)
  invisible(spectra_file)
}
