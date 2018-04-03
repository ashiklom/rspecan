library(rspecan)
library(tidyverse)
library(PEcAnRTM)
import::from("here", "here")

config_file <- here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

data_root <- here("raw_data/barnes")

project_code <- "barnes_2017"

project_list <- list(
  project_code = project_code,
  short_name = "Barnes 2017",
  long_name = paste0(
    "Barnes et al. 2017. ",
    "Beyond greenness: Detecting temporal changes in photosynthetic ",
    "capacity with hyperspectral reflectance data. ",
    "PLoS ONE."
  ),
  doi = "10.1371/journal.pone.0189539",
  URL = "https://osf.io/zur8e/",
  site_description = "University of Arizona Biosphere 2 Research Center",
  spectra_methods = list(
    calibration = "white panel reference",
    comment = paste(
      "All measurements on adaxial surface, avoiding midrib.",
      "Three measurements of each leaf lamina, 9 spectra per leaf.",
      "All measurements collected between 10:30am and 11:30am"
    )
  )
)

common_metadata <- tibble(
  site_code = "UA_B2RC",
  plot_code = "UA_B2RC",
  latitude = hms2dd(32, 34, 51, "N"),
  longitude = hms2dd(110, 50, 57, "W"),
  elevation = 1189,
  species_code = "PODE3",  # Populus deltoides
  Database_ID = "USDA Plants",
  instrument_code = "asd-fs3",
  apparatus = "leaf clip",
  is_experiment = TRUE
)

spec_files <- fs::path(data_root, "3_Spectral_data") %>% fs::dir_ls()

metadata_all <- tibble(filename = fs::path_file(spec_files)) %>%
  mutate(spectra_id = str_remove(filename, ".asd.txt$")) %>%
  separate(
    spectra_id,
    c("drop_all", "tag_leaf", "drop_leaf", "month", "day", "year_id"),
    remove = FALSE
  ) %>%
  mutate(
    tag_tree = str_extract(tag_leaf, "[[:alpha:]]"),
    day = as.numeric(day),
    month = as.numeric(month),
    year = str_extract(year_id, "^[[:digit:]]{4}") %>% as.numeric(),
    collection_date = as.POSIXct(ISOdate(year, month, day)),
    observation_id = paste(
      "barnes",
      tag_leaf,
      strftime(collection_date, "%Y-%m-%d"),
      sep = "_"
    )
  ) %>%
  select(-spectra_id, -filename, -day, -month, -drop_all, -drop_leaf) %>%
  select(observation_id, everything()) %>%
  add_column(!!!common_metadata) %>%
  metar::add_metadata(!!!project_list)

metadata <- distinct(metadata_all, observation_id, .keep_all = TRUE)

message("Reading spectra data")
spec_list <- map(spec_files, read_tsv, col_types = "nn")
message("Merging spectra data")
spec_data <- reduce(spec_list, full_join, by = "Wavelength")

spec_mat <- spec_data %>% select(-Wavelength) %>% as.matrix()
colnames(spec_mat) <- metadata_all$observation_id
waves <- pull(spec_data, "Wavelength")

spectra <- spectra(spec_mat, waves, spectra_types = "R")
spectra[spectra > 1 | spectra < 0] <- NA

message("Creating specdb project")
create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)
message("Done!")
