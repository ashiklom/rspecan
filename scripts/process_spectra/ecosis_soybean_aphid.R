library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)

data_name <- "ecosis_soybean_aphid"
data_longname <- "Productivity and Characterization of Soybean Foliar Traits Under Aphid Pressure"
ecosis_id <- "cdbb6b09-b481-4022-a0da-ad95a8b085d8"
ecosis_file <- sprintf(
  "https://ecosis.org/package/export?package_id=%s&metadata=true",
  ecosis_id
)

out_file <- "raw_data/soybean_aphin_pressure.csv"
if (file.exists(out_file)) {
  dat_raw <- read_csv(ecosis_file)
} else {
  message("Downloading data...")
  dat_raw <- read_csv(ecosis_file)
  write_csv(dat_raw, "raw_data/soybean_aphin_pressure.csv")
  message("Download complete!")
}

dat_full <- dat_raw %>%
  mutate(spectra_id = sprintf("%s_%03d", data_name, SAMP_ID))

############################################################
# Process spectra
############################################################
wave_rxp <- "^[[:digit:]]+$"
obs_ids <- dat_full$spectra_id
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
    observation_id = spectra_id,
    species_code = "GLMA4",
    Database_ID = "USDA Plants",
    leaf_cartot_per_area = add_metadata(CAROTENOIDS, data_unit = "ug cm-2"),
    leaf_chla_per_area = add_metadata(CHL_a, data_unit =  "ug cm-2"),
    leaf_chlb_per_area = add_metadata(CHL_b, data_unit = "ug cm-2"),
    leaf_chltot_per_area = leaf_chla_per_area + leaf_chlb_per_area %>%
      add_metadata(data_unit = "ug cm-2"),
    collection_date = ISOdate(YYYY, MM, DD),
    year = lubridate::year(collection_date),
    instrument_code = "asd-fs4",
    latitude = LATITUDE,
    longitude = LONGITUDE,
    sun_shade = recode(LEAF_HEIGHT, L = "shade", U = "sun"),
    soy_stage = STAGE,
    treatment_soy = TREAT,
    leaf_mass_per_area = add_metadata(gmm2_LMA, data_unit = "g m-2"),
    leaf_C_pct_mass = add_metadata(pct_CARBON, data_unit = "%"),
    leaf_cellulose_pct_mass = add_metadata(pct_CELLULOSE, data_unit = "%"),
    leaf_fiber_pct_mass = add_metadata(pct_FIBER, data_unit = "%"),
    leaf_lignin_pct_mass = add_metadata(pct_LIGNIN, data_unit = "%"),
    leaf_N_pct_mass = add_metadata(pct_NITROGEN, data_unit = "%"),
    is_agriculture = TRUE,
    is_experiment = TRUE
  ) %>%
  add_metadata(
    project_code = data_name,
    short_name = "Soybean aphid",
    long_name = data_longname,
    spectra_methods = list(comment = "Contact, leaf")
  )

create_project(
  specdb = "spectra_db",
  project_code = data_name,
  spectra = spectra,
  metadata = dat,
  overwrite = TRUE
)
