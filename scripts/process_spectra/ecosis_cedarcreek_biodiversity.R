library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)

ecosis_file <- "raw_data/2014-cedar-creek-esr-grassland-biodiversity-experiment--leaf-level-contact-data--trait-predictions.csv"

project_code <- "ecosis_cedarcreek_biodiversity"
dat_full <- read_csv(ecosis_file) %>%
  mutate(observation_id = sprintf("%s_%03d", project_code, ID))

############################################################
# Process spectra
############################################################
wave_rxp <- "^[[:digit:]]+$"
spectra_colname <- "spectra_id"

waves <- colnames(dat_full) %>% str_subset(wave_rxp) %>% as.numeric()
obs_ids <- dat_full$observation_id
spectra <- dat_full %>%
  select(matches(wave_rxp)) %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, "R")

############################################################
# Process metadata
############################################################
dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

species_dict <- tribble(
  ~`USDA Symbol`, ~species_code,
  "poapr", "POPR",
  "petpu", "DAPU5",
  "petca", "DACA7",
  "schsc", "SCSC",
  "petvi", "DAVI",
  "amoca", "AMCA6",
  "andge", "ANGE",
  "panvi", "PAVI2",
  "sornu", "SONU2",
  "koecr", "KOMA",
  "luppe", "LUPE3",
  "achmi", "ACMI2",
  "lesca", "LECA8",
  "asctu", "ASTU",
  "leppe", "LUPE3",
  "liaas", "LIAS",
  "monfi", "MOFI",
  "solri", "OLRIR"
)

metadata <- dat_sub %>%
  left_join(species_dict) %>%
  transmute(
    observation_id = observation_id,
    instrument_code = "asd-fs3",
    year = 2014,
    target_type = "leaf",
    leaf_mass_per_area = add_metadata(`LMA g m2`, data_unit = "g m-2"),
    leaf_C_pct_mass = add_metadata(`C`, data_unit = "%"),
    leaf_N_pct_mass = add_metadata(N, data_unit = "%"),
    leaf_CN_ratio = add_metadata(`C:N`, data_unit = ""),
    leaf_lignin_pct_mass = add_metadata(Lignin, data_unit = "%"),
    leaf_cellulose_pct_mass = add_metadata(`Cell`, data_unit = "%"),
    leaf_fiber_pct_mass = add_metadata(Fiber, data_unit = "%"),
    leaf_chltot_per_area = add_metadata(`Chl g m2`, data_unit = "g m-2"),
    species_code = species_code,
    Database_ID = "USDA Plants",
    latitude = 45.402,
    longitude = -93.199,
    is_experiment = FALSE
  ) %>%
  add_metadata(
    project_code = project_code,
    short_name = "Cedar Creek Biodiv.",
    long_name = "2014 Cedar Creek ESR Grassland Biodiversity Experiment: Leaf-level Contact Data: Trait Predictions",
    spectra_methods = list(
      comment = "Contact measurements, handheld, on leaf"
    )
  )

############################################################
# Store results
############################################################
create_project(
  specdb = "spectra_db",
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)
