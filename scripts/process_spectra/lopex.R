library(rspecan)
library(tidyverse)
library(udunits2)

config_file <- here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

data_path <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/curated-leafspec/data/lopex"
project_code <- "lopex"

project_metadata <- list(
  project_code = project_code,
  short_name = "LOPEX",
  long_name = "Leaf Optical Properties Experiment (1993)",
  URL = "http://opticleaf.ipgp.fr/index.php?page=database",
  site_description = "Joint Research Center, Ispra, Italy",
  spectra_methods = list(
    calibration = "spectralon ratio",
    comment = paste(
      "For more details, see",
      "http://teledetection.ipgp.jussieu.fr/opticleaf/lopex.htm#spectral"
    )
  )
)

common_metadata <- tibble(
  site_code = "ispra",
  plot_code = "ispra",
  latitude = 45.803,
  longitude = 8.630,
  year = 1993,
  instrument_code = "perkin-elmer-l19",
  apparatus = "integrating sphere",
  is_experiment = FALSE
)

# Set paths for LOPEX data
chem_path <- file.path(data_path, "LDB_lopex1993.csv")
spec_path <- file.path(data_path, "spec")

# Load main data.
species_info <- read_csv(here("extdata/species_dict/lopex_species_dict.csv"))
species_rxp <- "([[:alpha:]]{3})[[:alpha:]]* x? ?([[:alpha:]]{3})[[:alpha:]]* *.*"
lopex_raw <- read_csv(chem_path) %>%
  rename(raw_species_code = `Latin Name`) %>%
  select(-`Plant Type`, -`English Name`) %>%
  as_tibble() %>%
  mutate_if(is.numeric, na_if, y = -999) %>%
  fill(raw_species_code) %>%
  mutate(
    observation_id = sprintf("lopex_%02d", group_indices(., raw_species_code, C_a, C_b, C_car))
  ) %>%
  select(observation_id, everything())

lopex_traits <- lopex_raw %>%
  rename(
    leaf_C_pct_mass = C_C, leaf_H_pct_mass = C_H, leaf_O_pct_mass = C_O,
    leaf_N_pct_mass = C_N, leaf_prospect_N = N,
    leaf_chla_per_area = C_a, leaf_chlb_per_area = C_b,
    leaf_chltot_per_area = C_ab, leaf_cartot_per_area = C_car,
    leaf_anth_per_area = C_anth,
    leaf_mass_per_area = LMA, leaf_water_thickness = EWT,
    leaf_thickness = LT, leaf_area = A
  ) %>%
  mutate(
    leaf_CN_ratio_mass = leaf_C_pct_mass / leaf_N_pct_mass,
    leaf_protein_pct_mass = (C_prot1 + C_prot2) / 2,
    leaf_cellulose_pct_mass = (C_cell1 + C_cell2) / 2,
    leaf_lignin_pct_mass = (C_lign1 + C_lign2) / 2,
    leaf_starch_pct_mass = case_when(
      !is.na(C_star1) & !is.na(C_star2) ~ (C_star1 + C_star2) / 2,
      !is.na(C_star1) ~ C_star1,
      !is.na(C_star2) ~ C_star2,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    -C_prot1, -C_prot2, -C_cell1, -C_cell2, -C_lign1, -C_lign2,
    -C_star1, -C_star2, -FW, -DW
  ) %>%
  mutate_at(vars(dplyr::matches("pct_mass")), add_metadata, data_unit = "%") %>%
  mutate_at(vars(dplyr::matches("ratio")), add_metadata, data_unit = "") %>%
  mutate_at(vars(dplyr::matches("per_area")), add_metadata, data_unit = "ug cm-2") %>%
  add_column_metadata(
    leaf_prospect_N = list(data_unit = ""),
    leaf_mass_per_area = list(data_unit = "g cm-2"),
    leaf_water_thickness = list(data_unit = "g cm-2"),
    leaf_area = list(data_unit = "cm2"),
    leaf_thickness = list(data_unit = "um")
  )

species_info <- read_csv(here("extdata/species_dict/lopex_species_dict.csv"))
metadata_all <- lopex_traits %>%
  left_join(species_info) %>%
  add_column(!!!common_metadata)

dedup <- function(x) {
  if (is.numeric(x)) {
    out <- mean(x, na.rm = TRUE)
  } else {
    out <- unique(x)
  }
  stopifnot(length(out) == 1)
  if (sticky::is.sticky(x)) {
    mostattributes(out) <- attributes(x)
  }
  out
}

metadata <- metadata_all %>%
  select(-Refl_file, -Trans_file) %>%
  group_by(observation_id) %>%
  summarize_all(dedup) %>%
  distinct() %>%
  add_metadata(!!!project_metadata)

read_spec <- function(files, spectra_types) {
  map(file.path(spec_path, files), data.table::fread) %>%
    map(~spectra(.[[2]], .[[1]], spectra_types = spectra_types)) %>%
    reduce(cbind)
}

message("Reading spectra data...")
spectra <- metadata_all %>%
  select(observation_id, Refl_file, Trans_file) %>%
  mutate(
    reflectance = map(Refl_file, read_spec, spectra_types = "R"),
    transmittance = map(Trans_file, read_spec, spectra_types = "T"),
    spectra = map2(reflectance, transmittance, cbind),
    spectra = map2(spectra, observation_id, ~`colnames<-`(.x, rep(.y, 2)))
  ) %>%
  pull(spectra) %>%
  do.call(cbind, .)

message("Finalizing project")
create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = overwrite
)
