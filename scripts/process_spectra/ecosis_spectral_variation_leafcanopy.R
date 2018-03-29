library(tidyverse)
library(rspecan)
library(metar)
library(PEcAnRTM)

data_name <- "ecosis_spectral_variation_leafcanopy"
data_longname <- "Spectral Variation Between Leaf-level and Canopy-level Measurements"
ecosis_file <- "raw_data/spectral-variation-between-leaf-level-and-canopy-level-measurements.csv"

dat_full <- read_csv(ecosis_file) %>%
  # Keep only single plants
  filter(`# of Plants` == 1)

############################################################
# Process spectra
############################################################
wave_rxp <- "^[[:digit:]]+$"
obs_ids <- dat_full$idstr
waves <- colnames(dat_full) %>% str_subset(wave_rxp) %>% as.numeric()
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

dat <- dat_sub %>%
  transmute(
    observation_id = idstr,
    N_fertilization_treatment = `Fertilization Treatment`,
    species_code = Species,
    Database_ID = "USDA Plants",
    latitude = 43.08,
    longitude = -89.42,
    instrument_code = "asd-fs3",
    is_experiment = TRUE,
    is_agriculture = TRUE,
    year = 2013
  ) %>%
  add_metadata(
    project_code = data_name,
    short_name = "Spectral variation",
    long_name = data_longname,
    spectra_methods = list(comment = "Handheld, contact, greenhouse. Only using single plant spec.")
  )

create_project(
  specdb = "spectra_db",
  project_code = data_name,
  spectra = spectra,
  metadata = dat,
  overwrite = TRUE
)
