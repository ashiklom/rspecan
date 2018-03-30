#' Quickly create relative paths to files and directories
#'
#' @param ... List of paths, as in [fs::path]
#' @return File name, as character
#' @export
infile <- function(...) {
  args <- parse_path_args(...)
  path <- do.call(fs::path, args)
  file_dir <- fs::path_dir(path) %>% fs::dir_create()
  path
}

#' @rdname infile
#' @export
indir <- function(...) {
  args <- parse_path_args(...)
  path <- do.call(fs::path, args) %>% fs::dir_create()
  path
}

parse_path_args <- function(...) {
  args <- rlang::list2(...)
  base_dir <- rprojroot::find_root("DESCRIPTION")
  if (!any(grepl(base_dir, args))) {
    args <- c(base_dir, args)
  }
  args
}
