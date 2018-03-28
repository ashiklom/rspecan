library(rspecan)
library(tidyverse)
library(PEcAnRTM)
library(metar)

project_code <- "ecosis_milkweed_stress"

ecosis_id <- "9425d5b2-7633-45b5-9c07-6ec3323499a0"
ecosis_file <- sprintf(
  "https://ecosis.org/package/export?package_id=%s&metadata=true",
  ecosis_id
)
out_file <- "raw_data/milkweed_leaf_responses_to_water_stress_and_elevated_temperature.csv"

data_shortname <- "Milkweed stress"
data_longname <- "Common Milkweed Leaf Responses to Water Stress and Elevated Temperature"

if (file.exists(out_file)) {
  dat_raw <- read_csv(out_file)
} else {
  message("Downloading data...")
  dat_raw <- read_csv(ecosis_file)
  write_csv(dat_raw, out_file)
  message("Download complete!")
}

dat_full <- dat_raw %>%
  mutate(observation_id = sprintf("%s_%03d_%02d", project_code, ID, rep_ID))

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
spectra[spectra < 0] <- NA

############################################################
# Process metadata
############################################################
dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    observation_id = observation_id,
    species_code = "ASSY",
    Database_ID = "USDA Plants",
    leaf_CN_ratio = add_metadata(`C:N`, data_unit = ""),
    leaf_fiber_pct_mass = add_metadata(`Fiber (% dm)`, data_unit = "%"),
    leaf_C_pct_mass = add_metadata(`Leaf carbon content per leaf area`, data_unit = "%"),
    leaf_mass_per_area = add_metadata(`Leaf mass per area`, data_unit = "g m-2"),
    leaf_N_pct_mass = add_metadata(`Leaf nitrogen content per leaf area (% dm)`, data_unit = "%"),
    leaf_lignin_pct_mass = add_metadata(`Lignin (% dm)`, data_unit = "%"),
    leaf_Vcmax_area = add_metadata(`Vcmax`, data_unit = "umol m-2 s-1"),
    treatment_water = recode(`Water treatment`, ww = "Well-watered", ws = "Water stressed"),
    treatment_temperature = Temperature,
    is_experiment = TRUE,
    instrument_code = "asd-fs3",
    latitude = 43.076,
    longitude = -89.422,
    year = 2012
  ) %>%
  add_metadata(
    project_code = project_code,
    short_name = data_shortname,
    long_name = data_longname,
    URL = ecosis_file,
    spectra_methods = list(comment = "Contact, on leaf. See ECOSIS.")
  )

############################################################
# Store results
############################################################
create_project(
  specdb = "spectra_db",
  project_code = project_code,
  spectra = spectra,
  metadata = dat,
  overwrite = TRUE
)
