library(rspecan)
library(tidyverse)
library(PEcAnRTM)
library(metar)
import::from("here", "here")

config_file <- here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

data_root <- here("raw_data/barnes")

project_code <- "barnes_2017"

project_list <- list(
  project_code = project_code,
  short_name = "Barnes 2017",
  long_name = paste0(
    "Barnes et al. 2017. ",
    "Beyond greenness: Detecting temporal changes in photosynthetic ",
    "capacity with hyperspectral reflectance data. ",
    "PLoS ONE."
  ),
  doi = "10.1371/journal.pone.0189539",
  URL = "https://osf.io/zur8e/",
  site_description = "University of Arizona Biosphere 2 Research Center",
  spectra_methods = list(
    calibration = "white panel reference",
    comment = paste(
      "All measurements on adaxial surface, avoiding midrib.",
      "Three measurements of each leaf lamina, 9 spectra per leaf.",
      "All measurements collected between 10:30am and 11:30am"
    )
  )
)

common_metadata <- tibble(
  site_code = "UA_B2RC",
  plot_code = "UA_B2RC",
  latitude = hms2dd(32, 34, 51, "N"),
  longitude = hms2dd(110, 50, 57, "W"),
  elevation = 1189,
  species_code = "PODE3",  # Populus deltoides
  Database_ID = "USDA Plants",
  instrument_code = "asd-fs3",
  apparatus = "leaf clip",
  is_experiment = TRUE
)

spec_data <- fs::path(data_root, "PROCESSED", "9_processed_hyperspectral_wide") %>%
  read_csv()

obs_ids <- spec_data$uniqueID
wave_rxp <- "X[[:digit:]]{3,4}"
waves <- colnames(spec_data) %>%
  str_subset(wave_rxp) %>%
  str_remove("^X") %>%
  as.numeric()

spectra <- spec_data %>%
  select(matches(wave_rxp)) %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, "R")

proc_data <- fs::path(data_root, "PROCESSED", "8_merged_dataset.csv") %>%
  read_csv() %>%
  select(
    observation_id = uniqueID,
    Plant_ID = Plant_ID.x,
    collection_date = Date.x,
    leaf_Vcmax_area = Vcmax,
    leaf_Jmax_area = Jmax,
    #leaf_Rd, <-- Dropping because unsure about the units
    #leaf_water_thickness = LWC, <- Dropping because unsure about the units
    leaf_water_potential = Water_Pot,
    leaf_temperature = Leaf_T,
    air_temperature = Air_T,
    delta_temperature = Delta_T,
    vapor_pressure_deficit = VPD,
    Genotype,
    Average_Thickness,
    Par_Total,
    Wind_Speed
  ) %>%
  mutate(
    collection_date = lubridate::mdy(collection_date),
    year = lubridate::year(collection_date)
  ) %>%
  add_column_metadata(
    leaf_Vcmax_area = list(data_unit = "umol m-2 s-1"),
    leaf_Jmax_area = list(data_unit = "umol m-2 s-1"),
    leaf_water_potential = list(data_unit = "MPa")
  )

metadata <- proc_data %>%
  add_column(!!!common_metadata) %>%
  add_metadata(!!!project_list)

message("Creating specdb project")
create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)
message("Done!")
