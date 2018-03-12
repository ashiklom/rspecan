spectra2list <- function(spectra, observation_id = colnames(spectra)) {
  spectra_list <- mat2list(spectra, observation_id) %>%
    map(~list(
      spectra = .,
      spectra_types = spectra_types(.),
      wavelengths = wavelengths(.)
    ))
  spectra_list
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
