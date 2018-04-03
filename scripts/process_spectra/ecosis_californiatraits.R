library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)

ecosis_file <- "raw_data/fresh-leaf-spectra-to-estimate-leaf-traits-for-california-ecosystems.csv"

project_code <- "ecosis_californiatraits"

project_metadata <- list(
  project_code = project_code,
  short_name = "Cali. Eco. Traits",
  long_name = "Fresh Leaf Spectra to Estimate Leaf Traits for California Ecosystems",
  spectra_methods = list(
    comment = "See ECOSIS"
  )
)

dat_full <- read_csv(ecosis_file)

wave_rxp <- "^[[:digit:]]+$"
dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

metadata <- dat_sub %>%
  transmute(
    # metadata
    observation_id = paste(project_code, `sample name`, spectra, sep = "."),
    spectra_type = recode(measurement, `REFL` = "R"),
    replicate = `Replicate`,
    collection_date = 41365 + lubridate::as_date("1900-01-01") %>% as.POSIXct(),
    year = lubridate::year(collection_date),
    latitude = Latitude,
    longitude = Longitude,
    instrument_code = recode(`Instrument Model`, `ASD FieldSpec` = "asd-fs"),
    species_code = `species`,
    Database_ID = "USDA Plants",
    is_experiment = FALSE,
    target_type = `Target Type`,
    leaf_age = `age`,
    # traits
    leaf_cellulose_pct_mass = add_metadata(Cellulose, data_unit = "%"),
    leaf_mass_per_area = add_metadata(`Leaf mass per area`, data_unit = "g m-2"),
    leaf_N_pct_mass = add_metadata(`Leaf nitrogen content per leaf dry mass`, data_unit = "%"),
    leaf_water_pct_mass = add_metadata(`Leaf relative water content`, data_unit = "%"),
    leaf_lignin_pct_mass = add_metadata(`Lignin`, data_unit = "%"),
    leaf_water_thickness = leaf_mass_per_area * leaf_water_pct_mass / 100 %>%
      add_metadata(data_unit = "g m-2")
  ) %>%
  group_by(latitude, longitude) %>%
  mutate(
    site_code = sprintf("%s.site_%03d", project_code, row_number()),
    plot_code = site_code
  ) %>%
  add_metadata(!!!project_metadata) %>%
  #group_by(observation_id) %>%
  #summarize_all(
    #~if_else(is.numeric(.), mean(., na.rm = TRUE), unique(.))
  #) %>%
  #add_column_metadata(
    #leaf_cellulose_pct_mass = list(data_unit = "%"),
    #leaf_mass_per_area = list(data_unit = "g m-2"),
    #leaf_N_pct_mass = list(data_unit = "%"),
    #leaf_water_pct_mass = list(data_unit = "%"),
    #leaf_lignin_pct_mass = list(data_unit = "%")
  #) %>%
  glimpse()

obs_ids <- metadata$observation_id
waves <- colnames(dat_full) %>% str_subset(wave_rxp) %>% as.numeric()
spectra <- dat_full %>%
  select(matches(wave_rxp)) %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, metadata$spectra_type)

create_project(
  specdb = "spectra_db",
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)
