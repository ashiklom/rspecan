#' @importFrom assertthat %has_name%
check_project_info <- function(project_info) {
  assertthat::assert_that(
    project_info %has_name% "short_name",
    project_info %has_name% "long_name"
  )
  invisible(project_info)
}
