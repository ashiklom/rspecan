#' Convert two columns of a data frame to a named vector
#' 
#' @param .data Data frame from which to extract names
#' @param values Column containing dictionary values
#' @param keys Column containing dictionary keys
#' @export
df2dict <- function(.data, values = 2, keys = 1) {
  out <- dplyr::pull(.data, !!values)
  nms <- dplyr::pull(.data, !!keys)
  names(out) <- nms
  out
}

#' Swap the keys and values of a named vector
#'
#' @param dict A named vector
#' @export
swap_names <- function(dict) {
  stopifnot(!is.null(names(dict)))
  new_keys <- unname(dict)
  new_vals <- names(dict)
  names(new_vals) <- new_keys
  new_vals
}
