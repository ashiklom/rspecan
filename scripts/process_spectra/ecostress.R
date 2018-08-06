#!/usr/bin/env Rscript
import::from(drake, drake_plan, drake_config, make, loadd, new_cache, file_in)
import::from(magrittr, "%>%")
pkgconfig::set_config("drake::strings_in_dots" = "literals")

dir.create(".process_caches", showWarnings = FALSE)
cache <- new_cache(".process_caches/ecostress")

## ECOSTRESS data are stored in plain text files, with one spectrum per file.
## In each spectrum file, the first 20 lines are metadata, followed by a blank line, and then the data themselves, in the form "wavelength <tab> reflectance".

#' Convert block of "Variable: value" into `tibble` with each
#' "Variable" in its own column.
#'
#' @param raw_meta String containing "Variable: value" specifications
#' @return `tibble` with each "Variable" in its own column
#' @author Alexey Shiklomanov
parse_metadata <- function(raw_meta) {
  meta_split <- stringr::str_split_fixed(raw_meta, ":", 2)
  meta_colnames <- stringr::str_trim(meta_split[, 1])
  meta_values <- stringr::str_trim(meta_split[, 2])
  names(meta_values) <- meta_colnames
  tibble::tibble(!!!meta_values)
}

#' Read metadata from ECOSTRESS spectra file
#'
#' @param filename Path to ECOSTRESS spectra file
#' @return `tibble` of metadata, with each variable in its own column.
#'   Also includes `spectra_id` (unique spectra identifier), filename,
#'   and number of lines skipped.
#' @author Alexey Shiklomanov
read_raw_metadata <- function(filename) {
  raw_meta_full <- readLines(filename, n = 100)
  end_metadata <- grep("^[[:space:]]*$", raw_meta_full)
  if (length(end_metadata) != 1) {
    print(raw_meta_full)
    stop("Bad number of endpoints.")
  }
  raw_meta <- raw_meta_full[seq_len(end_metadata - 1)]

  meta_tbl <- parse_metadata(raw_meta) %>%
    dplyr::mutate(
      spectra_id = paste("ECOSTRESS", `Sample No.`, sep = "__"),
      observation_id = spectra_id,
      filename = filename,
      nskip = end_metadata
    )

  meta_tbl
}

#' The `Origin` column contains geographic information in the format
#' `latitude; longitude; CRS`. This separates this into the
#' corresponding columns.
#'
#' @param origin_string String containing "Origin" specification (character, length 1)
#' @return `tibble` containing `latitude`, `longitude` (both numeric),
#'   and `CRS` (character)
#' @author Alexey Shiklomanov
parse_origin <- function(origin_string) {
  coord_list <- stringr::str_split(origin_string, ";")
  if (length(coord_list[[1]]) != 3) {
    return(tibble::tibble(
      latitude = NA,
      longitude = NA,
      CRS = NA
    ))
  }
  tibble::tibble(
    latitude = purrr::map_chr(coord_list, 1) %>% as.numeric(),
    longitude = purrr::map_chr(coord_list, 2) %>% as.numeric(),
    CRS = purrr::map_chr(coord_list, 3) %>% stringr::str_trim()
  )
}

#' Read ECOSTRESS spectral data.
#'
#' Reads the actual spectra data, which are tab-separated and start on
#' the line given by `nskip` (returned as part of the output of
#' [read_spectra_metadata]).
#'
#' @param spectra_metadata `tibble` containing spectra metadata, as
#'   returned by [read_spectra_metadata].
#' @return Long `tibble` containing spectra data (each row is
#'   wavelength x observation).
#' @author Alexey Shiklomanov
read_spectra_data <- function(spectra_metadata) {
  spec_sub <- spectra_metadata %>%
    dplyr::select(filename, nskip, spectra_id, Measurement)

  spec_nest <- spec_sub %>%
    dplyr::mutate(
      spectra_tbl = purrr::map2(
        filename,
        nskip,
        ~readr::read_tsv(.x, skip = .y, col_names = c("wavelength", "value"),
                         col_types = "dd")
      )
    )

  spec_nest %>%
    tidyr::unnest() %>%
    dplyr::select(spectra_id, spectra_type = Measurement, wavelength, value) %>%
    dplyr::mutate(
      observation_id = spectra_id,
      wavelegth = wavelength * 1000,    # convert from um to nm
      value = value / 100               # convert from % to 0-1
    )
}

## This function reads the additional metadata files that come with each spectrum.

#' Read ECOSTRESS ancillary files
#'
#' Note that these contain the same data as the `spectra_metadata`, so
#' they are not used here. Add the following to the drake plan to read these:
#'
#' `ancillary_files = list.files(raw_data_dir, "vegetation\\..*\\.ancillary.txt")`
#' `ancillary_files_full = file.path(raw_data_dir, ancillary_files)`
#' `ancillary_data = purrr::map_dfr(ancillary_files_full, read_ancillary)`
#'
#' @param filename Name of file (character, length 1)
#' @return `tibble` with one row, and columns named according to metadata labels
#' @author Alexey Shiklomanov
read_ancillary <- function(filename) {
  raw_meta <- readLines(filename) %>%
    grep("^ *$", ., value = TRUE, invert = TRUE)
  parse_metadata(raw_meta) %>%
    dplyr::mutate(spectra_id = paste("ECOSTRESS", `Sample No.`, sep = "__"))
}

plan <- drake_plan(
  raw_data_dir = "raw_data/ECOSTRESS/vegetation",
  spectra_files = list.files(raw_data_dir, "vegetation\\..*\\.spectrum.txt"),
  spectra_files_full = file.path(raw_data_dir, spectra_files),
  spectra_metadata_raw = purrr::map_dfr(spectra_files_full, read_raw_metadata),
  spectra_metadata = spectra_metadata_raw %>%
    dplyr::mutate(
      coords = suppressWarnings(purrr::map(Origin, parse_origin))
    ) %>%
    tidyr::unnest(coords),
  spectra_data = read_spectra_data(spectra_metadata),
  species_info = readr::read_csv(file_in("spectra_db/species_info.csvy")),
  species_local = readr::read_csv(file_in("extdata/species_dict/ecostress_species_dict.csv"))
)

plan_config <- drake_config(plan, cache = cache)
make(plan, cache = cache)
if (interactive()) loadd(cache = cache)

if (FALSE) {
  metadata_clean <- spectra_metadata %>%
    dplyr::left_join(species_local, by = c("Genus" = "genus", "Species" = "species")) %>%
    dplyr::filter(!is.na(species_code))

  metadata_clean %>%
    dplyr::transmute(
      project_code = "ecostress",
      observation_id = spectra_id,
      latitude = latitude,
      longitude = longitude,
      species_code = species_code,
      species_code_type = species_code_type,
      instrument_code = "beckman-uv5240",
      apparatus = "integrating sphere"
    )
}
