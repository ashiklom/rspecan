#' Data frame of PROSPECT parameters
#'
#' @export
variable_df <- tibble::tribble(
  ~code, ~shortname, ~short_units,
  "N", "# meso", expression("# meso." ~ "(unitless)"),
  "Cab", "Chl.", expression("Chl." ~ (mu*g ~ cm^{-2})),
  "Car", "Car.", expression("Car." ~ (mu*g ~ cm^{-2})),
  "Canth", "Anth.", expression("Anth." ~ (mu*g ~ cm^{-2})),
  "Cw", "Water", expression("Water" ~ (g ~ cm^{-2})),
  "Cm", "Dry matter", expression("Dry matter" ~ (g ~ cm^{-2}))
)
