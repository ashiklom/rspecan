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
  cn <- colnames(spectra)
  stopifnot(
    is_spectra(spectra) || !is.null(rn),
    !is.null(cn)
  )
  if (is_spectra(spectra)) {
    wl <- wavelengths(spectra)
  } else if (!is.null(rn)) {
    wl <- as.numeric(rn)
  }
  spectra_ids <- cn
  hfile <- h5::h5file(spectra_file)
  if (overwrite) {
    h5::h5unlink(hfile, data_name)
  }
  hfile[data_name] <- spectra
  h5::h5attr(hfile[data_name], "wavelengths") <- wl
  h5::h5attr(hfile[data_name], "spectra_id") <- spectra_ids
  h5::h5close(hfile)
  invisible(spectra_file)
}
