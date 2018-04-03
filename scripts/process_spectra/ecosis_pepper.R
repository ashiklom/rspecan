library(tidyverse)
library(metar)
library(PEcAnRTM)
library(rspecan)

data_name <- "ecosis_pepper"
data_longname <- "Fresh and Dry Pepper Leaf Spectra with Associated Potassium and Nitrogen Measurements"
ecosis_id <- "a67925bf-f715-449a-939c-3cb000fb7889"
ecosis_file <- sprintf(
  "https://ecosis.org/package/export?package_id=%s&metadata=true",
  ecosis_id
)
out_file <- "raw_data/pepper_leaf_spectra_potassium_nitrogen.csv"
if (file.exists(out_file)) {
  dat_raw <- read_csv(out_file)
} else {
  dat_raw <- read_csv(ecosis_file)
  write_csv(dat_raw, out_file)
}

dat_full <- dat_raw %>%
  group_by(PlantNumber) %>%
  mutate(spectra_id = sprintf("%s_%02d_%02d", data_name, PlantNumber, row_number())) %>%
  ungroup()

############################################################
# Process spectra
############################################################
wave_rxp <- "^[[:digit:]]+$"
waves <- colnames(dat_full) %>% str_subset(wave_rxp) %>% as.numeric()
obs_ids <- dat_full$spectra_id
st <- recode(
  dat_full$Measurement_Type,
  "Fresh Transmittance" = "T",
  "Fresh Reflectance" = "R",
  "Fresh Absorbance" = "A",
  "Dry Reflectance" = "R"
)
spectra <- dat_full %>%
  select(matches(wave_rxp)) %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, st)

############################################################
# Process metadata
############################################################
dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    observation_id = spectra_id,
    species_code = "CAAN4",
    Database_ID = "USDA Plants",
    fresh_dry = case_when(
      grepl("^Fresh", Measurement_Type) ~ "fresh",
      grepl("^Dry", Measurement_Type) ~ "dry",
      TRUE ~ NA_character_
    ),
    year = 2014,
    latitude = 31.997,  # Volcani center -- not 100% sure if this is where spectra were actually collected
    longitude = 34.817,
    instrument_code = "asd-fs",
    leaf_N_pct_mass = add_metadata(`Leaf nitrogen content per leaf dry mass (% DW)`, data_unit = "%"),
    leaf_K_pct_mass = add_metadata(`Leaf potassium content per leaf dry mass (% DW)`, data_unit = "%"),
    is_experiment = TRUE
  ) %>%
  add_metadata(
    project_code = data_name,
    short_name = "Pepper K/N",
    long_name = data_longname,
    spectra_methods = list(comment = "Contact, leaf. Reflectance with LC, transmittance with IS")
  )

############################################################
# Store results
############################################################
create_project(
  specdb = "spectra_db",
  project_code = data_name,
  spectra = spectra,
  metadata = dat,
  overwrite = TRUE
)
