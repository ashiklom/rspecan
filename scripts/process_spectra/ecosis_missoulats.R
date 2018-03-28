library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)

project_code <- "ecosis_missoulats"
data_shortname <- "Missoula TS"
data_longname <- "Missoula Montana lodgepole pine & big sagebrush time series"
ecosis_file <- "raw_data/missoula-montana-lodgepole-pine---big-sagebrush-time-series.csv"
dat_full <- read_csv(ecosis_file)

wave_rxp <- "^[[:digit:]]+$"
dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    observation_id = Spectra,
    collection_date = lubridate::mdy(`Sample Collection Date`),
    year = lubridate::year(collection_date),
    latitude = `Latitude`,
    longitude = `Longitude`,
    species_code = `USDA Symbol`,
    Database_ID = "USDA Plants",
    species_genus = `Latin Genus`,
    species_species = `Latin Species`,
    species_common_name = `Common Name`,
    elevation = `Elevation (m)`,
    elevation_unit = "m",
    description = `Full Description`,
    leaf_ash_pct_mass = add_metadata(`% Ash`, data_unit = "%"),
    leaf_fat_pct_mass = add_metadata(`% Fat`, data_unit = "%"),
    leaf_fiber_pct_mass = add_metadata(`% Neutral detergent fiber`, data_unit = "%"),
    leaf_NSC_pct_mass = add_metadata(`% Non-structural carbohydrate`, data_unit = "%"),
    leaf_protein_pct_mass = add_metadata(`% Protein`, data_unit = "%"),
    leaf_water_pct_mass = add_metadata(`Relative water content` * 100, data_unit = "%"),
    instrument_code = "asd-fs4",
    apparatus = "integrating sphere",
    is_experiment = FALSE
  ) %>%
  add_metadata(
    project_code = project_code,
    short_name = data_shortname,
    long_name = data_longname,
    doi = "10.21232/C2D08D",
    spectra_methods = list(comment = "Contact, leaf, IS, see ECOSIS")
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
  project_code = project_code,
  spectra = spectra,
  metadata = dat,
  overwrite = TRUE
)
