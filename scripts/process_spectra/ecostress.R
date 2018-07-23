## ECOSTRESS
## :PROPERTIES:
## :header-args:R: :comments both :tangle scripts/process_spectra/ecostress.R :results output replace drawer :session *R*
## :END:

## #+NAME: setup

## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::setup][setup]]
import::from(drake, drake_plan, drake_config, make, loadd)
import::from(magrittr, "%>%")
pkgconfig::set_config("drake::strings_in_dots" = "literals")
options(crayon.enabled = FALSE)
## setup ends here



## ECOSTRESS data are stored in plain text files, with one spectrum per file.
## In each spectrum file, the first 20 lines are metadata, followed by a blank line, and then the data themselves, in the form "wavelength <tab> reflectance".

## #+NAME: read_spectra

## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::read_spectra][read_spectra]]
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

  tidyr::unnest(spec_nest) %>%
    dplyr::select(spectra_id, spectra_type = Measurement, wavelength, value)
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
  spectra_data = read_spectra_data(spectra_metadata)
)

plan_config <- drake_config(plan)
make(plan)
## read_spectra ends here



## #+RESULTS: read_spectra
## :RESULTS:

## target spectra_data
## Warning: target spectra_data warnings:
##   number of columns of result is not a multiple of vector length (arg 1)
##   16 parsing failures.
## row # A tibble: 5 x 5 col     row col   expected  actual   file                                           expected   <int> <chr> <chr>     <chr>    <chr>                                          actual 1     1 <NA>  2 columns 4 colum… 'raw_data/ECOSTRESS/vegetation/vegetation.gra… file 2     2 <NA>  2 columns 4 colum… 'raw_data/ECOSTRESS/vegetation/vegetation.gra… row 3     3 <NA>  2 columns 4 colum… 'raw_data/ECOSTRESS/vegetation/vegetation.gra… col 4     4 <NA>  2 columns 4 colum… 'raw_data/ECOSTRESS/vegetation/vegetation.gra… expected 5     5 <NA>  2 columns 4 colum… 'raw_data/ECOSTRESS/vegetation/vegetation.gra…

## ... ............................................................................... ........ ............................................................................... ...... ............................ [... truncated]
## Warning messages:
## 1: In rbind(names(probs), probs_f) :
##   number of columns of result is not a multiple of vector length (arg 1)
## 2: In rbind(names(probs), probs_f) :
##   number of columns of result is not a multiple of vector length (arg 1)
## 3: In rbind(names(probs), probs_f) :
##   number of columns of result is not a multiple of vector length (arg 1)
## :END:


## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::*ECOSTRESS][ECOSTRESS:3]]
loadd(spectra_metadata)

filename <- spectra_files_full[1]

spectra_metadata <- purrr::map_dfr(
  spectra_files_full,
  read_spectra_metadata
)
## ECOSTRESS:3 ends here
