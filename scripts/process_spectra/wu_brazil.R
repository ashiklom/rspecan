library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)
library(readxl)
library(lubridate)

config_file <- here::here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

datapath <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/Jin_Wu_Brazil_Data"

project_code <- "wu_brazil"

project_metadata <- list(
  project_code = project_code,
  short_name = "Wu 2016",
  long_name = "Wu et al. 2016 New Phytologist canopy traits study",
  doi = "10.1111/nph.14051",
  spectra_methods = list(
    calibration = "Spectralon ratio",
    specmethodcomment = "Chavana-Bryant et al. 2016; doi:10.1111/nph.13853"
  )
)

spec_raw <- file.path(datapath, 'Brazil_ASD_Leaf_Spectra_filter_v1.csv') %>%
  read_csv() %>%
  mutate(wavelength = str_remove(Wavelength, " nm") %>% as.numeric()) %>%
  select(-Wavelength)

waves <- pull(spec_raw, "wavelength")
obs_ids <- paste0("wu_brazil_", spec_raw %>% select(-wavelength) %>% colnames())
spectra <- spec_raw %>%
  select(-wavelength) %>%
  as.matrix() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, "R")
spectra[spectra < 0] <- NA

species_info <- file.path(datapath, "Brazil_Species_Name.xlsx") %>%
  read_excel() %>%
  mutate(
    observation_id = paste0("wu_brazil_L", row_number())
  ) %>%
  left_join(read_csv("extdata/species_dict/wu_brazil_species_dict.csv"))

metadata <- file.path(datapath, "Brazil_Trait_Data_filter_v1.csv") %>%
  read_csv() %>%
  mutate(
    observation_id = paste0("wu_brazil_", Leaf_Number)
  ) %>%
  left_join(species_info) %>%
  mutate(
    leaf_mass_per_area = add_metadata(1 / SLA_m2_kg, data_unit = "kg m-2"),
    leaf_water_pct_mass = add_metadata(Water_Perc, data_unit = "%"),
    leaf_water_thickness = leaf_mass_per_area * leaf_water_pct_mass %>%
      add_metadata(data_unit = "kg m-2"),
    collection_date = mdy(Day),
    year = year(collection_date),
    canopy_position = recode(
      Light_Environment,
      `3` = "T",
      `2` = "M",
      `1` = "B"
    ),
    complete_leaf = recode(
      Complete_Leaf,
      `1` = "complete",
      `2` = "non-complete"
    ),
    leaf_age = recode(
      Leaf_Age,
      `1` = "young",
      `2` = "mature",
      `3` = "old"
    ),
    site_code = "Brazil",
    plot_code = site_code,
    latitude = (-2 - 51 / 60),
    longitude = (-54 - 58 / 60),
    instrument_code = "asd-fspro",
    is_experiment = FALSE
  ) %>%
  select(-SLA_m2_kg, -Water_Perc, -Light_Environment,
         -Complete_Leaf, -Leaf_Age, -Leaf_Number) %>%
  add_metadata(!!!project_metadata)

create_project(
  specdb = specdb,
  project_code = project_code,
  metadata = metadata,
  spectra = spectra,
  overwrite = TRUE
)
