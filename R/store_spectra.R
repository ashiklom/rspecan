store_spectra <- function(spec_data, spectra, observation_id = colnames(spectra)) {
  spectra_list <- mat2list(spectra, observation_id)
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
