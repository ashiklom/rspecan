library(rspecan)
library(tidyverse)
library(PEcAnRTM)
library(metar)

ecosis_file <- "raw_data/hawaii-2000-vegetation-species-spectra.csv"
dat_full <- read_csv(ecosis_file)

project_code <- "ecosis_hawaii2000"

wave_rxp <- "^[[:digit:]]+$"

dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

metadata <- dat_sub %>%
  transmute(
    species_common_name = `Common Name`,
    description = `Description`,
    species_genus = case_when(
      !is.na(`Latin genus`) ~ `Latin genus`,
      !is.na(`Latin Genus`) ~ `Latin Genus`,
      TRUE ~ NA_character_
    ),
    species_species = case_when(
      !is.na(`Latin species`) ~ `Latin species`,
      !is.na(`Latin Species`) ~ `Latin Species`,
      TRUE ~ NA_character_
    ),
    latitude = Latitude,
    longitude = Longitude,
    collection_date = lubridate::mdy(`Sample Collection Date`),
    year = lubridate::year(collection_date),
    observation_id = `Spectra`,
    target_type = `Surface Type`,
    species_code = `USDA Symbol`,
    Database_ID = "USDA Plants",
    instrument_code = "asd-fs",
    is_experiment = FALSE
  ) %>%
  add_metadata(
    project_code = project_code,
    short_name = "Hawaii 2000",
    long_name = "Hawaii 2000 vegetation species spectra",
    spectra_methods = list(comment = "Proximal, not handheld. Various target types. See Ecosis.")
  )

waves <- colnames(dat_full) %>% str_subset(wave_rxp) %>% as.numeric()
obs_ids <- metadata$observation_id
spectra <- dat_full %>%
  select(matches(wave_rxp)) %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, "R")
spectra <- spectra[[c(352:1810, 1940:2476)]]

create_project(
  specdb = "spectra_db",
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)
