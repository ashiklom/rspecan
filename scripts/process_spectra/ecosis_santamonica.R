library(rspecan)
library(tidyverse)
library(PEcAnRTM)
library(metar)

data_name <- "ecosis_santamonica"
data_longname <- "Santa Monica Mountains vegetation species spectra"
ecosis_file <- "raw_data/santa-monica-mountains-vegetation-species-spectra.csv"

dat_full <- read_csv(ecosis_file)

wave_rxp <- "^[[:digit:]]+$"
dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    observation_id = Spectra,
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
    species_code = `USDA Symbol`,
    Database_ID = "USDA Plants",
    collection_date = lubridate::mdy(`Sample Collection Date`),
    year = lubridate::year(collection_date),
    target_type = `Surface Type`,
    instrument_code = "asd-fs",
    is_experiment = FALSE,
    latitude = 34.08,
    longitude = -118.80
  ) %>%
  add_metadata(
    project_code = data_name,
    short_name = "Santa Monica Mtns.",
    long_name = data_longname,
    spectra_methods = list(comment = "Proximal, handheld, some canopy"),
  )

waves <- colnames(dat_full) %>% str_subset(wave_rxp) %>% as.numeric()
obs_ids <- dat$observation_id
spectra <- dat_full %>%
  select(matches(wave_rxp)) %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, "R")

create_project(
  specdb = "spectra_db",
  project_code = data_name,
  spectra = spectra,
  metadata = dat,
  overwrite = TRUE
)
