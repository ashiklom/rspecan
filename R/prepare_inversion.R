#' Prepare an inversion for submission
#'
#' @export
prepare_inversion <- function(specdb, prospect_version,
                              inversion_path = strftime(Sys.time(), "invert/%Y%m%d-%H%M%S"),
                              overwrite_all = FALSE) {
  status_table <- get_status_table(specdb, prospect_version)
  if (overwrite_all) {
    status_table$status <- "none"
  }
  status_sub <- status_table %>%
    dplyr::filter(status %in% c("none", "not_converged"))

  fs::dir_create(inversion_path)
  write_inversion_table(status_sub, fs::path(inversion_path, "invert_queue"))

  logdir <- fs::path(inversion_path, "log")
  fs::dir_create(logdir)

  # Write submit script
  ninv <- nrow(status_sub)
  stopifnot(ninv > 0)
  submit_script <- c(
    "#!/usr/bin/env bash",
    "#$ -q 'geo*'",
    "#$ -j y",
    paste("#$ -o", logdir),
    "#$ -N spec_invert",
    paste0("#$ -t 1-", ninv),
    "Rscript scripts/02_run_inversion.R $SGE_TASK_ID"
  )

  submit_path <- fs::path(inversion_path, "submit.sh")
  writeLines(submit_script, submit_path)
  fs::file_chmod(submit_path, "u+rwx")
}

#' Build a table of information about status of current inversions
#'
#' @inheritParams run_inversion
#' @export
get_status_table <- function(specdb, prospect_version, spectra_types = c("R", "PA", "CRR")) {
  get_metadata(specdb, include_metadata = FALSE) %>%
    dplyr::select(project = project_code, observation_id) %>%
    dplyr::mutate(
      specdb = specdb,
      unique_id = paste(project, observation_id, sep = ".")
    ) %>%
    dplyr::left_join(
      tidyr::expand(., tidyr::crossing(unique_id, prospect_version)),
      by = "unique_id"
    ) %>%
    dplyr::mutate(
      status = purrr::pmap_chr(
        list(
          specdb = specdb,
          project = project,
          observation_id = observation_id,
          prospect_version = prospect_version
        ),
        get_inversion_status
      )
    ) %>%
    tibble::add_column(spectra_types = list(spectra_types))
}

#' @export
write_inversion_table <- function(status_table, filename, ...) {
  status_table %>%
    dplyr::select(project, observation_id, specdb, prospect_version, spectra_types) %>%
    dplyr::mutate(
      spectra_types = purrr::map_chr(spectra_types, paste, collapse = "|")
    ) %>%
    readr::write_csv(path = filename, ...)
}

#' @export
read_inversion_table <- function(filename, i) {
  readr::read_csv(filename) %>%
    dplyr::mutate(spectra_types = strsplit(spectra_types, split = "\\|")) %>%
    dplyr::slice(i)
}
