## ECOSTRESS
## :PROPERTIES:
## :header-args:R: :comments both :tangle scripts/process_spectra/ecostress.R :results output replace drawer :session *R*
## :END:

## #+NAME: setup

## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::setup][setup]]
import::from(drake, drake_plan, drake_config, make, loadd, new_cache)
import::from(magrittr, "%>%")
pkgconfig::set_config("drake::strings_in_dots" = "literals")

dir.create(".process_caches", showWarnings = FALSE)
cache <- new_cache(".process_caches/ecostress")
## setup ends here



## ECOSTRESS data are stored in plain text files, with one spectrum per file.
## In each spectrum file, the first 20 lines are metadata, followed by a blank line, and then the data themselves, in the form "wavelength <tab> reflectance".

## This first set of functions parses out the metadata from each spectra file.

## #+NAME: read spectra metadata functions

## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::read%20spectra%20metadata%20functions][read spectra metadata functions]]
read_raw_metadata <- function(filename) {
  raw_meta_full <- readLines(filename, n = 100)
  end_metadata <- grep("^[[:space:]]*$", raw_meta_full)
  if (length(end_metadata) != 1) {
    print(raw_meta_full)
    stop("Bad number of endpoints.")
  }
  raw_meta <- raw_meta_full[seq_len(end_metadata - 1)]
  meta_split <- stringr::str_split_fixed(raw_meta, ":", 2)
  meta_colnames <- stringr::str_trim(meta_split[, 1])
  meta_values <- stringr::str_trim(meta_split[, 2])
  names(meta_values) <- meta_colnames

  meta_tbl <- tibble::tibble(!!!meta_values) %>%
    dplyr::mutate(
      spectra_id = paste("ECOSTRESS", `Sample No.`, sep = "__"),
      filename = filename,
      nskip = end_metadata
    )

  meta_tbl
}
## read spectra metadata functions ends here



## #+RESULTS: read spectra metadata functions
## :RESULTS:
## :END:

## The ~Origin~ column contains geographic information in the format ~latitude; longitude; CRS~.
## The ~parse_origin~ function separates this into the corresponding columns.


## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::*ECOSTRESS][ECOSTRESS:3]]
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
## ECOSTRESS:3 ends here



## #+RESULTS:
## :RESULTS:
## :END:

## This function reads the actual spectra data, which are tab-separated and start on the line given by ~nskip~ (returned as part of the output of ~read_spectra_metadata~).
## The output here is a long data frame suitable for ~fst~ storage.

## #+NAME: read spectra data function

## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::read%20spectra%20data%20function][read spectra data function]]
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
    dplyr::mutate(value = value / 100)
}
## read spectra data function ends here



## #+RESULTS: read spectra data function
## :RESULTS:
## :END:

## This function reads the additional metadata files that come with each spectrum.


## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::*ECOSTRESS][ECOSTRESS:5]]
## ECOSTRESS:5 ends here



## Finally, the ~drake~ plan for processing the data.

## #+NAME: drake plan

## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::drake%20plan][drake plan]]
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
  ancillary_files = list.files(raw_data_dir, "vegetation\\..*\\.ancillary.txt")
)

plan_config <- drake_config(plan, cache = cache)
make(plan, cache = cache)
## drake plan ends here

loadd(cache = cache)
spectra_metadata
