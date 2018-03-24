library(rspecan)
library(tidyverse)
library(udunits2)
library(metar)
library(PEcAnRTM)

config_file <- here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

data_path <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/curated-leafspec/data/angers"
project_code <- "angers"

project_metadata <- list(
  project_code = "angers",
  short_name = "ANGERS",
  long_name = "Angers, France spectra from INRA",
  URL = "http://opticleaf.ipgp.fr/index.php?page=database",
  site_description = "National Institute for Agricultural Research (INRA), Angers, France",
  spectra_methods = list(
    calibration = "unknown",
    comment = "For more details, see ECOSIS project website: https://ecosis.org/#result/2231d4f6-981e-4408-bf23-1b2b303f475e"
  )
)

common_metadata <- tibble(
  site_code = "INRA",
  plot_code = "INRA",
  latitude = 47.47,
  longitude = -0.56,
  year = 2003,
  instrument_code = "asd-fs",
  apparatus = "leaf_clip",
  is_experiment = FALSE
)

meta_path <- file.path(data_path, "LDB_angers2003.csv")
spec_path <- file.path(data_path, "spec")

meta_raw <- read_csv(meta_path) %>%
  select(-`English Name`) %>%
  rename(
    species_data_code = `Latin Name`,
    leaf_prospect_N = N,
    leaf_chla_per_area = C_a,
    leaf_chlb_per_area = C_b,
    leaf_chltot_per_area = C_ab,
    leaf_cartot_per_area = C_car,
    leaf_anth_per_area = C_anth,
    leaf_water_thickness = EWT,
    leaf_mass_per_area = LMA
  ) %>%
  add_column_metadata(
    leaf_prospect_N = list(data_unit = ""),
    leaf_chla_per_area = list(data_unit = "ug cm-2"),
    leaf_chlb_per_area = list(data_unit = "ug cm-2"),
    leaf_chltot_per_area = list(data_unit = "ug cm-2"),
    leaf_cartot_per_area = list(data_unit = "ug cm-2"),
    leaf_anth_per_area = list(data_unit = "ug cm-2"),
    leaf_water_thickness = list(data_unit = "g cm-2"),
    leaf_mass_per_area = list(data_unit = "g cm-2")
  ) %>%
  mutate(observation_id = str_remove(Refl_file, "\\.txt$"))

species_info <- read_csv(here("extdata/species_dict/angers_species_dict.csv"))

metadata <- meta_raw %>%
  left_join(species_info) %>%
  add_column(!!!common_metadata) %>%
  select(-`Plant Type`, -`T660`, -`T720`, -`T840`, -`T940`, -`T1300`,
         -Refl_file, -Trans_file, -species_data_code, -SPAD) %>%
  add_metadata(!!!project_metadata)

read_spec <- function(files, spectra_types) {
  map(file.path(spec_path, files), data.table::fread) %>%
    map(~spectra(.[[2]], .[[1]], spectra_types = spectra_types)) %>%
    reduce(cbind)
}

spectra <- meta_raw %>%
  select(observation_id, Refl_file, Trans_file) %>%
  mutate(
    reflectance = map(Refl_file, read_spec, spectra_types = "R"),
    transmittance = map(Trans_file, read_spec, spectra_types = "T"),
    spectra = map2(reflectance, transmittance, cbind),
    spectra = map2(spectra, observation_id, ~`colnames<-`(.x, rep(.y, 2)))
  ) %>%
  pull(spectra) %>%
  do.call(cbind, .)

spectra[spectra < 0] <- NA

create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = overwrite
)
