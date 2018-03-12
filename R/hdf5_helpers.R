#' Open a group, or create it if it doesn't exist
#'
#' @param hfile HDF5 file object (see [hdf5r::H5File]).
#' @param group Name of group, as character
#' @param overwrite Logical. If `TRUE`, unlink the target first.
#' @return H5File object of the resulting group
#' @export
h5_group <- function(hfile, group, overwrite = FALSE) {
  if (hfile$exists(group) && overwrite) {
    hfile$link_delete(group)
  }
  if (hfile$exists(group)) {
    hfile[group]
  } else {
    hfile$create_group(group)
  }
}

#' Open a dataset, or create it if it doesn't exist
#'
#' @inheritParams h5_group
#' @param dataset Name of target dataset, as character
#' @param data R object to store in dataset
#' @export
h5_dataset <- function(hfile, dataset, data = NULL, overwrite = FALSE) {
  if (hfile$exists(dataset) && overwrite) {
    hfile$link_delete(dataset)
  }
  if (hfile$exists(dataset)) {
    hfile[dataset]
  } else {
    hfile$create_dataset(dataset, data)
  }
}

#' Recursively insert a list into an HDF5 file
#'
#' @inheritParams h5_group
#' @param l A named
#' @export
list2hdf <- function(hfile, l, overwrite = FALSE) {
  stopifnot(
    !is.null(names(l)),
    all(names(l) != "")
  )
  for (i in seq_along(l)) {
    li <- l[[i]]
    namei <- names(l)[i]
    if (is.list(li) && !is.data.frame(li)) {
      h2 <- h5_group(hfile, namei, overwrite = overwrite)
      .h <- list2hdf(h2, li, overwrite = overwrite)
    } else {
      h2 <- h5_dataset(hfile, namei, li, overwrite = overwrite)
    }
  }
  hfile
}

#' Sanitize data for HDF5 storage
#'
#' @param data `data.frame` or `list` to sanitize
sanitize_df <- function(data) {
  data %>%
    mutate_if(is.character, ~stringi::stri_trans_general(., "latin-ascii"))
}
