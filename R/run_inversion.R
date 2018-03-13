#' Invert a spectrum from the spectra database
#'
#' @param specdb_file Path to spectra file
#' @inheritParams load_spectra
#' @inheritParams PEcAnRTM::invert_fieldspec
#' @param overwrite Logical. If `TRUE`, overwrite all information related to 
#' this run. Default = `FALSE.`
#' @param ... Additional parameters to [PEcAnRTM::invert_fieldspec]
#' @export
run_inversion <- function(project,
                          observation_id,
                          specdb_file,
                          prospect_version,
                          spectra_types = c("R", "PA", "CRR"),
                          overwrite = FALSE,
                          ...) {

  h5_group2 <- purrr::partial(h5_group, hfile = specdb_file)

  h5_invert <- specdb_file %>%
    h5_group(group = h5_path(project, "inversion"), overwrite = FALSE) %>%
    h5_path(., observation_id) %>%
    h5_group2(group = ., overwrite = FALSE) %>%
    h5_path(., paste0("prospect_", prospect_version)) %>%
    h5_group2(group = ., overwrite = TRUE)

  if (h5_exists(specdb_file, h5_invert, "status") &&
      h5_get_data(specdb_file, h5_invert, "status") != "not_converged") {
    stop(
      "This run currently has a status: ", h5_invert[["status"]][],
      ". If you want to start a new run anyway, set 'overwrite = TRUE'."
    )
  }

  observed <- load_spectra(project, observation_id, specdb_file,
                           spectra_types = spectra_types)

  set_inversion_status(specdb_file, h5_invert, "running")

  interrupted <- tryCatch({
    samples <- PEcAnRTM::invert_fieldspec(observed, prospect_version = prospect_version, ...)
    FALSE
  }, interrupt = function(e) {
    message("Caught user interrupt.")
    TRUE
  }
  )

  if (interrupted) {
    message("User interrupt. Deleting run link.")
    specdb$link_delete(h5_path(project, "inversion", observation_id))
    specdb$close_all()
    stop("Terminating because of user interrupt.")
  }

  samples_matrix <- BayesianTools::getSample(samples, parametersOnly = FALSE)
  h5_invert[["samples"]] <- samples_matrix

  samples_coda <- BayesianTools::getSample(samples, coda = TRUE)
  samples_burned <- PEcAn.assim.batch::autoburnin(samples_coda, TRUE)

  if (samples_burned$burnin == 1) {
    message("Run did not converge. Marking status as 'not_converged'.")
    set_inversion_status(h5_invert, "not_converged")
  } else {
    message("Run converged. Marking status as 'complete'.")
    set_inversion_status(h5_invert, "complete")
    param_summary <- summary(samples_burned$samples)[c("statistics", "quantiles")] %>%
      purrr::map(tibble::as_tibble, rownames = "parameter") %>%
      purrr::reduce(dplyr::left_join, by = "parameter")
    h5_invert[["results"]] <- param_summary
    h5_invert[["burnin"]] <- samples_burned$burnin
  }

  message("Inversion complete!")
  invisible(TRUE)
}

set_inversion_status <- function(hfile, path, status) {
  status_levels <- c("running", "complete", "not_converged")
  assertthat::assert_that(
    assertthat::is.string(status),
    status %in% status_levels
  )
  status_fac <- factor(status, status_levels)
  h5_dataset(hfile, h5_path(path, "status"), status_fac, overwrite = TRUE)
  invisible(TRUE)
}
