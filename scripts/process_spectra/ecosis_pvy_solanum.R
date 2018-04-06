library(tidyverse)
library(rspecan)
library(metar)
library(PEcAnRTM)

data_name <- "ecosis_pvy_solanum"
data_longname <- "Varietal Discrimination and Detection of PVY in Solanum tuberosum: Hawaii 2014"
ecosis_file <- "raw_data/varietal-discrimination-and-detection-of-pvy-in-solanum-tuberosum--hawaii-2014.csv"

dat_full <- read_csv(ecosis_file) %>%
  mutate(
    spectra_id = sprintf("%s_%04d_%02d", data_name, ID, rep)
  )

wave_rxp <- "^[[:digit:]]+$"
waves <- colnames(dat_full) %>% str_subset(wave_rxp) %>% as.numeric()
obs_id <- dat_full$spectra_id
spectra <- dat_full %>%
  select(matches(wave_rxp)) %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_id) %>%
  spectra(waves, "R")

dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    observation_id = spectra_id,
    PVY_infected = recode(infect, h = "healthy", i = "infected"),
    variety = VAR,
    species_genus = "Solanum",
    species_species = "tuberosum",
    species_code = "SOTU",
    Database_ID = "USDA Plants",
    is_experiment = TRUE,
    year = 2014,
    latitude = 21.31,
    longitude = -157.86,
    instrument_code = "svc-hr"
  ) %>%
  add_metadata(
    project_code = data_name,
    short_name = "PVY solanum",
    long_name = data_longname,
    spectra_methods = list(comment = "Contact, leaf reflectance")
  )

create_project(
  specdb = "spectra_db",
  project_code = data_name,
  spectra = spectra,
  metadata = dat,
  overwrite = TRUE
)
