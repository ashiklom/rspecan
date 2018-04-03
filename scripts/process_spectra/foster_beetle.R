library(rspecan)
library(tidyverse)
library(PEcAnRTM)
library(metar)

config_file <- here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

datapath <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/Foster_etal/SBMP2_contrem_all.csv"

project_code <- "foster_beetle"
project_metadata <- list(
  project_code = project_code,
  short_name = "Foster 2017",
  long_name = "Foster et al. 2017 FEM",
  doi = "10.1016/j.foreco.2016.11.004",
  site_description = paste("Monarch Pass,", "Sawatch Range,", 
                          "Grand Mesa-Uncompahgre-Gunnison National Forests,",
                          "Salida, CO"),
  spectra_methods = list(
    comment = "See manuscript"
  )
)

site_info <- tribble(
  ~site_code, ~latitude, ~longitude,
  "MP_CO", hms2dd(38, 30, 10.08, "N"), hms2dd(106, 20, 8.1594, "W")
) %>%
  mutate(plot_code = site_code)

rawdat <- read_csv(datapath)

meta1 <- rawdat %>%
  mutate(
    observation_id = paste("foster_beetle", TREEID, Height, Infested, sep = "|"),
    species_code = "PIEN",
    Database_ID = "USDA Plants",
    site_code = "MP_CO",
    year = 2014,
    collection_date = as.POSIXct("2014-09-05"),
    sun_shade = factor(Height, c("L", "H")) %>% lvls_revalue(c("shade", "sun")),
    bark_beetle_infested = factor(Infested, c("Y", "N")) %>% lvls_revalue(c("yes", "no")),
    instrument_code = "asd-fspro",
    apparatus = "Leaf clip",
    is_experiment = FALSE
  )

metadata <- meta1 %>%
  select(-Height, -Infested, -matches("^[[:digit:]]+$")) %>%
  left_join(site_info) %>%
  add_metadata(!!!project_metadata)

obs_ids <- metadata %>% pull(observation_id)
spec_df <- meta1 %>%
  select(matches("^[[:digit:]]+$"))

waves <- colnames(spec_df) %>% as.numeric()
spectra <- spec_df %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, "CRR")

create_project(
  specdb = specdb,
  project_code = project_code,
  metadata = metadata,
  spectra = spectra,
  overwrite = TRUE
)
