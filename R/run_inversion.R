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
                          specdb,
                          prospect_version,
                          spectra_types = c("R", "PA", "CRR"),
                          overwrite = FALSE,
                          ...) {

  invert_path <- fs::path(specdb, project, "inversion", observation_id,
                          paste0("prospect_", prospect_version))
  fs::dir_create(invert_path, recursive = TRUE)

  curr_status <- get_inversion_status(specdb, project, observation_id, prospect_version)

  if (overwrite) {
    message("Overwriting run, whose previous status was: ", curr_status)
    set_inversion_status(specdb, project, observation_id, prospect_version, "none")
    curr_status <- "none"
  }

  if (!curr_status %in% c("none", "not_converged")) {
    stop(
      "This run currently has status: ", curr_status,
      ". If you want to start a new run anyway, set 'overwrite = TRUE'."
    )
  }

  observed <- load_spectra(project, observation_id, specdb,
                           spectra_types = spectra_types)

  info_file <- fs::path(invert_path, "info")
  options(inversion_log = info_file)

  write_info("start_time: ", as.character(Sys.time()), append = FALSE)
  write_info("project_code: ", project)
  write_info("observation_id: ", observation_id)
  write_info("prospect_version: ", prospect_version)
  write_info("input_spectra_types: ", paste(spectra_types, collapse = " "))
  spec_types <- PEcAnRTM::spectra_types(observed) %>% paste(collapse = " ")
  write_info("observed_spectra_types: ", spec_types)
  waves <- PEcAnRTM::wavelengths(observed)
  write_info("observed_ncol: ", ncol(observed))
  write_info("observed_wl_min: ", min(waves))
  write_info("observed_wl_max: ", max(waves))
  write_info("observed_nwl: ", nrow(observed))
  write_info("hostname: ", system2("hostname", stdout = TRUE))
  write_info("run_directory: ", getwd())

  tryCatch({
    set_inversion_status(specdb, project, observation_id, prospect_version, "running")
    samples <- PEcAnRTM::invert_fieldspec(observed, prospect_version = prospect_version)
    FALSE
  }, interrupt = function(e) {
    message("Caught user interrupt. Setting inversion status to `none`")
    set_inversion_status(specdb, project, observation_id, prospect_version, "none")
    write_info("end_time_sampling:", as.character(Sys.time()))
    write_info("status: Run terminated via user interrupt")
    stop("Terminating execution because of user interrupt.")
  }
  )

  write_info("end_time_sampling:", as.character(Sys.time()))

  samples_matrix <- BayesianTools::getSample(samples, parametersOnly = FALSE)
  saveRDS(samples_matrix, fs::path(invert_path, "samples.rds"))

  samples_coda <- BayesianTools::getSample(samples, coda = TRUE)
  samples_burned <- PEcAn.assim.batch::autoburnin(samples_coda, TRUE, method = "gelman.plot")

  if (samples_burned$burnin == 1) {
    message("Run did not converge. Marking status as 'not_converged'.")
    set_inversion_status(specdb, project, observation_id, prospect_version, "not_converged")
    write_info("status: Not converged after ", nrow(samples_matrix), " iterations")
  } else {
    message("Run converged. Marking status as 'complete'.")
    set_inversion_status(specdb, project, observation_id, prospect_version, "complete")
    write_info("status: Converged after ", nrow(samples_matrix), " iterations")
    write_info("burnin: ", samples_burned$burnin)
    param_summary <- summary(samples_burned$samples)[c("statistics", "quantiles")] %>%
      purrr::map(tibble::as_tibble, rownames = "parameter") %>%
      purrr::reduce(dplyr::left_join, by = "parameter") %>%
      metar::add_metadata(
        project_code = project,
        observation_id = observation_id,
        prospect_version = prospect_version,
        spectra_types = spectra_types
      )
    metar::write_csvy(param_summary, fs::path(invert_path, "results.csvy"))
  }

  message("Inversion complete!")
  invisible(TRUE)
}

#' Set and retrieve the status of an inversion
#'
#' @inheritParams run_inversion
#' @param status Character indicating status
#' @export
set_inversion_status <- function(specdb, project, observation_id, prospect_version, status) {
  status_dir <- fs::path(
    specdb, project, "inversion",
    observation_id, paste0("prospect_", prospect_version)
  )
  fs::dir_create(status_dir)
  statusfile <- fs::path(status_dir, "status")
  status_levels <- c("running", "complete", "not_converged", "none")
  assertthat::assert_that(
    assertthat::is.string(status),
    status %in% status_levels
  )
  if (status == "none") {
    file.remove(statusfile, showWarnings = FALSE)
  } else {
    writeLines(status, statusfile)
  }
  invisible(TRUE)
}

#' @rdname get_inversion_status
#' @export
get_inversion_status <- function(specdb, project, observation_id, prospect_version) {
  statusfile <- file.path(specdb, project, "inversion",
                          observation_id, paste0("prospect_", prospect_version), "status")
  if (!file.exists(statusfile)) {
    return("none")
  }
  readLines(statusfile)
}

#' Write log info about a run
#'
#' @param ... String to be pasted together and written out
#' @param file Output file. Defaults to `getOption("inversion_log")` or, if that's unset, the R console
#' @inheritParams base::cat
#' ... Additional arguments to [base::cat()]
write_info <- function(..., file = getOption("inversion_log"), append = TRUE) {
  if (is.null(file)) file <- ""
  cat(..., "\n", file = file, append = append, sep = "")
}
