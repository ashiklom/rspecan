#' Set value to NA if it matches some condition
#'
#' @param x Value to censor
#' @param cond Logical vector of conditions at which to censor
#' @export
censor_if <- function(x, cond) {
  x[cond] <- NA
  x
}

