#' Open a group, or create it if it doesn't exist
#'
#' @param hfile H5File object, or path to HDF5 file.
#' @param group Name of group, as character
#' @param overwrite Logical. If `TRUE`, unlink the target first.
#' @export
h5_group <- function(hfile, group, overwrite = FALSE) {
  UseMethod("h5_group", hfile)
}

#' @rdname h5_group
#' @export
h5_group.H5RefClass <- function(hfile, group, overwrite = FALSE) {
  if (hfile$exists(group) && overwrite) {
    hfile$link_delete(group)
  }
  if (hfile$exists(group)) {
    hfile[[group]]
  } else {
    hfile$create_group(group)
  }
}

#' @rdname h5_group
#' @export
h5_group.character <- function(hfile, group, overwrite = FALSE) {
  on.exit(hfile2$close_all())
  hfile2 <- h5_open(hfile)
  out <- h5_group.H5RefClass(hfile2, group, overwrite)
  invisible(out$get_obj_name())
}

#' Open a dataset, or create it if it doesn't exist
#'
#' @inheritParams h5_group
#' @param dataset Name of target dataset, as character
#' @param data R object to store in dataset
#' @export
h5_dataset <- function(hfile, dataset, data = NULL, overwrite = FALSE) {
  UseMethod("h5_dataset", hfile)
}

#' @rdname h5_dataset
#' @export
h5_dataset.H5RefClass <- function(hfile, dataset, data = NULL, overwrite = FALSE) {
  if (hfile$exists(dataset) && overwrite) {
    hfile$link_delete(dataset)
  }
  if (hfile$exists(dataset)) {
    hfile[[dataset]]
  } else {
    hfile$create_dataset(dataset, data)
  }
}

#' @rdname
#' @export
h5_dataset.character <- function(hfile, dataset, data = NULL, overwrite = FALSE) {
  on.exit(hfile2$close_all())
  hfile2 <- h5_open(hfile)
  out <- h5_dataset.H5RefClass(hfile2, dataset, data, overwrite)
  invisible(out$get_obj_name())
}

#' Recursively insert a list into an HDF5 file
#'
#' @inheritParams h5_group
#' @param l A named list
#' @export
list2hdf <- function(hfile, l, overwrite = FALSE) {
  UseMethod("list2hdf", hfile)
}

#' @rdname list2hdf
#' @export
list2hdf.H5RefClass <- function(hfile, l, overwrite = FALSE) {
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
      if (is.data.frame(li)) {
        li <- sanitize_df(li)
      }
      h2 <- h5_dataset(hfile, namei, li, overwrite = overwrite)
    }
  }
  hfile
}

#' @rdname list2hdf
#' @export
list2hdf.character <- function(hfile, l, overwrite = FALSE) {
  on.exit(hfile2$close_all())
  hfile2 <- h5_open(hfile)
  out <- list2hdf.H5RefClass(hfile2, l, overwrite = overwrite)
  invisible(out$get_obj_name())
}

#' Sanitize data for HDF5 storage
#'
#' @param data `data.frame` to sanitize
#' @return Sanitized `data.frame`
sanitize_df <- function(data) {
  data %>%
    mutate_if(is.character, ~stringi::stri_trans_general(., "latin-ascii"))
}

#' [base::file.path] analog for HDF5
h5_path <- function(...) {
  paste(..., sep = "/")
}

#' Check if object is an H5File
#'
#' @param x Object to check
#' @return Logical
#' @export
is.H5File <- function(x) {
  inherits(x, "H5File")
}

assertthat::on_failure(is.H5File) <- function(call, env) {
  paste0(deparse(call$x), " is not class H5File.")
}

#' Try to open an HDF5 file
#'
#' This is meant to try to circumvent issues with multiple R sessions opening 
#' the same HDF5 file.
#'
#' @param filename Path to HDF5 file
#' @param sleep Seconds to sleep between attempts (default = 0.2)
#' @param tries Number of attempts (default = 100)
#' @param ... Additional arguments to `H5File$new`
#' @export
h5_open <- function(filename, sleep = 0.2, tries = 100, ...) {
  assertthat::assert_that(
    assertthat::is.string(filename)
  )
  i <- 1
  while(i <= tries) {
    out <- tryCatch(
      hdf5r::H5File$new(filename, ...),
      error = function(e) NULL
    )
    if (is.null(out)) {
      i <- i + 1
      Sys.sleep(runif(1, sleep - 0.05, sleep + 0.05))
    } else {
      return(out)
    }
  }
  stop("Unable to open file ", filename,
       " after ", tries, " attempts.")
}

#' Coerce object into H5File
#'
#' For characters, open an H5File object. For `H5File`s, return the object 
#' itself.
#'
#' @param x Object to coerce to H5File
#' @return H5File object
#' @export
as.H5File <- function(x, ...) {
  UseMethod("as.H5File")
}

#' @rdname as.H5File
#' @export
as.H5File.character <- function(x, ...) {
  assertthat::assert_that(
    assertthat::is.string(x)
  )
  h5_open(x, ...)
}

#' @rdname as.H5File
#' @export
as.H5File.H5File <- function(x, ...) {
  x
}

#' Check if tag exists
#'
#' @inheritParams h5_path
#' @export
h5_exists <- function(hfile, ...) {
  dots <- list(...)
  hp <- do.call(h5_path, dots)
  hf <- h5_open(hfile)
  on.exit(hf$close_all())
  hf$exists(hp)
}

#' Read dataset
#'
#' @inheritParams h5_path
#' @export
h5_get_data <- function(hfile, ...) {
  stopifnot(h5_exists(hfile, ...))
  dots <- list(...)
  hp <- do.call(h5_path, dots)
  hf <- h5_open(hfile)
  on.exit(hf$close_all())
  hf[[hp]][]
}
