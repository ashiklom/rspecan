#' Read spectra from HDF5 file
#'
#' Return spectra from a given data set, wavelength (optional), and spectra_id (optional).
#'
#' If `wavelength` is passed directly as `X:Y`, then all wavelengths between X 
#' and Y are selected, even if they are not contiguous. For instance, if only 
#' every 5th wavelength is stored, passing `wavelength = 400:450` will return 
#' all wavelengths between 400 and 450. This behavior is only triggered if a 
#' `:` appears in the entered argument -- i.e. this behavior is NOT triggered 
#' if `wl <- 400:2500; read_spectra(..., wavelength = wl)`. Specifying 
#' `exact_wl = TRUE` disables this behavior.
#'
#' @inheritParams write_spectra
#' @param data_name Name of data in HDF5 file. If multiple data names are 
#' passed, spectra are combined via `cbind.spectra`
#' @param wavelength Wavelengths to return. If `NULL`, return all wavelengths.
#' @param spectra_id Spectra ID to return, as character. If `NULL`, return all spectra
#' @param exact_wl Logical, whether or not to parse wavelength specially. See details.
#' @param wl_range Numeric, length 2, indicating the minimum and maxiumum 
#' wavelength to use. Ignored if `wavelength` is not `NULL`.
#' @export
read_spectra <- function(spectra_file, data_name, wavelength = NULL, spectra_id = NULL,
                         exact_wl = FALSE, wl_range = NULL) {
  if (length(data_name) > 1) {
    spectra_list <- lapply(
      data_name,
      function(x) read_spectra(
        spectra_file = spectra_file,
        data_name = x,
        wavelength = wavelength,
        spectra_id = spectra_id,
        exact_wl = exact_wl,
        wl_range = wl_range
      )
    )
    return(do.call(cbind, spectra_list))
  }
  hfile <- h5::h5file(spectra_file)
  data_spec <- paste0(data_name, "/spectra")
  data_wl <- paste0(data_name, "/wavelengths")
  data_id <- paste0(data_name, "/spectra_id")
  ids <- hfile[data_id][]
  wl <- hfile[data_wl][]
  hdata <- hfile[data_spec]
  if (!is.null(wavelength)) {
    wl_ds <- deparse(substitute(wavelength))
    if (grepl(":", wl_ds) && !exact_wl) {
      wl_min <- min(wavelength)
      wl_max <- max(wavelength)
      i <- which(wl >= wl_min & wl <= wl_max)
    } else {
      i <- which(wl %in% wavelength)
    }
  } else if (!is.null(wl_range)) {
    stopifnot(length(wl_range) == 2)
    i <- which(wl >= wl_range[1] & wl <= wl_range[2])
  } else {
    i <- seq_along(wl)
  }
  if (!is.null(spectra_id)) {
    j <- which(ids %in% spectra_id)
  } else {
    j <- seq_along(ids)
  }
  rawspec <- hdata[i, j]
  colnames(rawspec) <- ids[j]
  spectra(rawspec, wl[i])
}
