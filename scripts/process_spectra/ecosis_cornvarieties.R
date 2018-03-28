library(rspecan)
library(tidyverse)
library(PEcAnRTM)
library(metar)

project_code <- "ecosis_cornvarieties"

ecosis_id <- "c0e238ea-5b23-452c-bc40-f0cfe2c6f032"
ecosis_file <- sprintf(
  "https://ecosis.org/package/export?package_id=%s&filters=&metadata=true",
  ecosis_id
)

out_file <- "raw_data/corn-varieties-west-madison.csv"

if (!file.exists(out_file)) {
  message("Downloading data...")
  dat_raw <- read_csv(ecosis_file)
  write_csv(dat_raw, "raw_data/corn-varieties-west-madison.csv")
  message("Download complete!")
} else {
  dat_raw <- read_csv(out_file)
}

dat_full <- dat_raw %>%
  mutate(observation_id = sprintf("%s_%03d_%02d", project_code, ID, rep))

############################################################
# Process spectra
############################################################
wave_rxp <- "^[[:digit:]]+$"
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

metadata <- dat_sub %>%
  transmute(
    observation_id = observation_id,
    species_code = "ZEMA",
    Database_ID = "USDA Plants",
    variety = Variety,
    latitude = 43.0617,
    longitude = -89.532,
    instrument_code = "asd-fs3",
    is_experiment = FALSE,
    year = 2014
  ) %>%
  add_metadata(
    project_code = project_code,
    short_name = "Corn var.",
    long_name = "Spectral Characterization of Multiple Corn Varieties: West Madison Agricultural Station 2014",
    URL = ecosis_file,
    spectra_methods = list(comment = "see ECOSIS")
  )

############################################################
# Store results
############################################################

create_project(
  specdb = "spectra_db",
  project_code = project_code,
  metadata = metadata,
  spectra = spectra,
  overwrite = TRUE
)
