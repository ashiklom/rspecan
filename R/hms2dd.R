#' Convert hour-minute-second latitude/longitude data to decimal degrees
#'
#' @param h Hour coordinate
#' @param m Minute coordinate
#' @param s Second coordinate
#' @param hemi Hemisphere (e.g. "N", "E", "S", "W") as character 
#' @return Decimal degree values
#' @export
hms2dd <- function(h, m, s, hemi) {
  if (hemi %in% c("N", "E")) {
    s <- 1
  } else if (hemi %in% c("S", "W")) {
    s <- -1
  } else {
    stop("Unknown quadrant. Must be N, E, S, or W.")
  }
  s * (h + m / 60 + s / (60^2)) 
}
