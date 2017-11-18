#' @export
check_datalist <- function(datalist) {
  need_fields <- c(
    "data_name",
    "data_longname",
    "data_filename",
    "self_filename",
    "metadata",
    "spectra",
    "data_wl_inds",
    "prospect_wl_inds"
  )
  need_metadata <- c(
    "data_name",
    "spectra_id",
    "spectra_type"
  )
  waves <- getwl(datalist$spectra)
  stopifnot(
    all(need_fields %in% names(datalist)),
    all(need_metadata %in% colnames(datalist$metadata)),
    ncol(datalist$spectra) == nrow(datalist$metadata),
    length(data_wl_inds) == length(prospect_wl_inds),
    all(waves %% 1 == 0),
    !is.null(colnames(datalist$spectra)),
    !any(duplicated(colnames(datalist$spectra))),
    !any(duplicated(datalist$metadata$spectra_id)),
    all(colnames(datalist$spectra) %in% datalist$metadata$spectra_id)
  )
  message("Dataset has ", ncol(datalist$spectra), " spectra")
  message("The first few spectra IDs are: \n",
          paste(head(colnames(datalist$spectra), 10), collapse = ", "))
  message("The last few spectra IDs are: \n",
          paste(tail(colnames(datalist$spectra), 10), collapse = ", "))
  message("Minimum wavelength: ", min(waves))
  message("Maximum wavelength: ", max(waves))
  invisible(TRUE)
}
