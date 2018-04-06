#' Create legend based on a named color pallete
#'
#' @param color_pallete Named color pallete
#' @param leg_title Legend title
#' @param theme ggplot2 theme object with which to modify legend
#' @export
create_legend <- function(color_pallete, leg_title, theme = ggplot2::theme()) {
  dframe <- tibble::tibble(
    x = names(color_pallete),
    z = seq_along(color_pallete)
  )
  plt <- ggplot2::ggplot(dframe) +
    ggplot2::aes(x = x, y = z, fill = x) +
    ggplot2::geom_col() +
    ggplot2::guides(fill = ggplot2::guide_legend(title = leg_title)) +
    ggplot2::scale_fill_manual(values = color_pallete) +
    theme
  cowplot::get_legend(plt)
}
