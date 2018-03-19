spectra2list <- function(spectra, observation_id = colnames(spectra)) {
  spectra_list <- mat2list(spectra, observation_id) %>%
    purrr::map(
      ~`colnames<-`(., sprintf("%s.%03d", colnames(.), seq_len(ncol(.))))
    ) %>%
    purrr::map(spectra2tibble)
  spectra_list
}

spectra2tibble <- function(spectra) {
  spectra %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    tibble::add_column(wavelengths = PEcAnRTM::wavelengths(spectra)) %>%
    metar::add_metadata(spectra_types = PEcAnRTM::spectra_types(spectra)) %>%
    dplyr::select(wavelengths, everything())
}

#' Split a matrix-like object into a list of matrices based on the groups
mat2list <- function(mat, groupvec, group = unique(groupvec)) {
  stopifnot(
    ncol(mat) == length(groupvec),
    all(groupvec %in% group)
  )
  out <- list()
  for (g in group) {
    out[[g]] <- mat[, groupvec == g]
  }
  out
}
