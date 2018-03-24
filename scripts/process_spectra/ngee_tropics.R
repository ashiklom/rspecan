library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)
library(readxl)
library(data.table)

config_file <- here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

project_code <- "ngee_tropics"

project_metadata <- list(
  project_code = project_code,
  short_name = "NGEE Tropics",
  long_name = "Next Generation Ecosystem Experiment (NGEE) - Tropics",
  site_description = "Brazil",
  spectra_methods = list(
    comment = "Ask Shawn Serbin"
  )
)

path_ngt <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/NGEE-Tropics"

namesdict <- c(
  "LWP_bar" = "leaf_water_potential",
  "Species" = "RawSpecies",
  "Location" = "Site"
)

Location <- "Panama"
SampleYear <- 2016

path_year <- file.path(path_ngt, paste(Location, SampleYear, sep = "_"))
chemfile <- list.files(path_year, "Leaf_Samples", full.names = TRUE)

chemdat <- read_excel(chemfile, sheet = 4) %>%
  mutate(LWP_bar = as.numeric(LWP_bar)) %>%
  as_tibble() %>%
  mutate_if(is.numeric, na_if, y = -9999) %>%
  rename(
    species_data_code = Species,
    SampleName = Barcode,
  ) %>%
  mutate(
    leaf_water_potential = add_metadata(LWP_bar, data_unit = "bar"),
    sun_shade = factor(Canopy_position, c("sunlit", "understory")) %>% lvls_revalue(c("sun", "shade"))
  ) %>%
  select(
    SampleName, species_data_code, sun_shade,
    starts_with("leaf_", ignore.case = FALSE)
  )

specfile <- list.files(path_year, "leaf_spectra", full.names = TRUE)

spectra_raw <- read_excel(specfile) %>%
  filter(!(Spectra == "BNL11815" & Site == "PNM")) %>%
  mutate(
    collection_date = as.Date(strptime(Date, "%Y%m%d")),
    year = lubridate::year(collection_date),
    site_code = Site,
    instrument_code = dplyr::recode(Instrument, "SVC_HR-1024i" = "svc-hr"),
    observation_id = paste(project_code, Spectra, year, sep = "|")
  ) %>%
  select(
    observation_id,
    year,
    SampleName = Spectra,
    site_code,
    collection_date,
    instrument_code, starts_with("Wave_")
  )

# NOTE: 20 data points have no species codes associated with them

sitelatlon <- tribble(
  ~site_code, ~latitude, ~longitude,
  "NA", NA, NA,
  # NOTE: Approximate coordinates based on Google Maps location of San 
  # Lorenzo Protected Forest;  need more precise ones from Shawn
  "PNM", 9.25, -79.99,
  "SanLorenzo", 9.25, -79.99)

metadata <- chemdat %>%
  full_join(select(spectra_raw, -starts_with("Wave_"))) %>%
  mutate(
    year = SampleYear,
    project_code = project_code,
    observation_id = paste(project_code, SampleName, year, sep = "|"),
    plot_code = site_code,
    is_experiment = FALSE
  ) %>%
  left_join(read_csv("extdata/species_dict/ngee_tropics_species_dict.csv")) %>%
  left_join(sitelatlon) %>%
  add_metadata(!!!project_metadata)

obs_ids <- pull(spectra_raw, observation_id)
spec_df <- spectra_raw %>%
  select(starts_with("Wave_"))
waves <- colnames(spec_df) %>% str_remove("Wave_") %>% as.numeric()
spectra <- spec_df %>%
  as.matrix() %>%
  t() %>%
  `*`(0.01) %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, "R")

create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)

# Useful species list from Smithsonian Tropical Research Institute:
# http://www.stri.si.edu/sites/esp/tesp/plant_species.htm
