#` ---
#` title: FFT data processing
#' author: Alexey Shiklomanov
#' ---

library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)

config_file <- here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

project_code <- "nasa_fft"

project_metadata <- list(
  project_code = project_code,
  short_name = "NASA FFT",
  long_name = "NASA Forest Functional Types (FFT)",
  URL = "https://ecosis.org/#result/4a63d7ed-4c1e-40a7-8c88-ea0deea10072",
  site_description = "Midwest and Mid-atlantic forests, USA",
  spectra_methods = list(
    calibration = "Spectralon ratio"
  )
)

root_path <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/NASA_FFT_Project"
spec_path <- file.path(root_path, "spec")
refl_path <- file.path(spec_path, "NASA_FFT_LC_Refl_Spectra_v4.csv")
trans_path <- file.path(spec_path, "NASA_FFT_IS_Tran_Spectra_v4.csv")

raw_refl <- read_csv(refl_path)
raw_trans <- read_csv(trans_path)

all_raw <- bind_rows(raw_refl, raw_trans)

all_data <- all_raw %>%
  mutate(
    observation_id = paste(project_code, Sample_Name, Sample_Year, sep = "|")
  ) %>%
  rename(
    sample_name = Sample_Name,
    year = Sample_Year,
    canopy_position = Height,
    site_code = Site,
    plot_code = Plot,
    species_data_code = Species
  ) %>%
  mutate(
    instrument_code = dplyr::recode(Instrumentation, `ASD FieldSpec 3` = "asd-fs3"),
    apparatus = dplyr::recode(
      Measurement_Type,
      `LC` = "Leaf clip",
      `IS` = "Integrating sphere"
    ),
    spectra_types = dplyr::recode(Measurement, `REFL` = "R", `TRAN` = "T")
  ) %>%
  select(-Instrumentation, -Measurement_Type, -Measurement)

obs_ids <- pull(all_data, observation_id)

raw_spec <- all_data %>%
  select(starts_with("Wave_"))
waves <- colnames(raw_spec) %>% str_remove("Wave_") %>% as.numeric()

spectra <- raw_spec %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(pull(all_data, observation_id)) %>%
  spectra(waves, spectra_types = pull(all_data, spectra_types))
spectra[spectra < 0] <- NA

meta_raw <- all_data %>% select(-starts_with("Wave_"))

meta2 <- meta_raw %>%
  rename(calibration = Calibration) %>%
  mutate(
    canopy_position = factor(canopy_position, c("B", "M", "T")),
    sun_shade = fct_recode(canopy_position, sun = "T", shade = "M", shade = "B"),
    needle_old_new = factor(Age, c("N", "O")),
    needle_age = factor(Age, c("N", 2, 3)) %>% as.numeric() %>% `-`(1),
    target_type = dplyr::recode(Sample_Type, Vegetation = "leaf"),
  ) %>%
  select(-Age, -Site_Name, -State, -Spectra, -Height_Age,
         -Project, -PI, -Affiliation, -Sample_Type,
         -Wavelengths, -Column_Units)

path_d15N <- file.path(root_path, "NASA_FFT_d15N_ANALYZED_DATA_UPDATED_4R.csv")
path_lignin <- file.path(root_path, "NASA_FFT_FIB_LIG_CELL_RESULTS_FINAL_4R.csv")
path_CN <- file.path(root_path, "NASA_FFT_Project_CN_Data_4R.csv")
path_SLA_LMA <- file.path(root_path, "NASA_FFT_SLA_LMA_Data_v2_4R_updated_new.csv")

raw_d15N <- read_csv(path_d15N) %>%
  select(
    sample_name = SAMPLE_NAME,
    year = SAMPLE_YEAR,
    species_data_code = SPECIES,
    leaf_dN15 = SAMPLE_dN15
  )

raw_lignin <- read_csv(path_lignin) %>%
  select(
    sample_name = SAMPLE_NAME,
    year = SAMPLE_YEAR,
    species_data_code = SPECIES,
    leaf_lignin_pct_mass = ADL_PERC_DW,
    leaf_fiber_pct_mass = ADF_PERC_DW
  )

raw_cn <- read_csv(path_CN) %>%
  select(
    sample_name = Sample_Name,
    year = Sample_Year,
    species_data_code = Species,
    leaf_N_pct_mass = Perc_N,
    leaf_C_pct_mass = Perc_C,
    leaf_CN_ratio_mass = CNRatio
  )

raw_lma <- read_csv(path_SLA_LMA) %>%
  select(
    sample_name = Sample_Name,
    year = Sample_Year,
    species_data_code = Species,
    leaf_mass_per_area = LMA_g_DW_m2,
    leaf_water_thickness = EWT_g_cm2
  )

dedup <- . %>%
  group_by(sample_name, year, species_data_code) %>%
  summarize_all(mean, na.rm = TRUE)

traits <- list(raw_d15N, raw_lignin, raw_cn, raw_lma) %>%
  map(dedup) %>%
  reduce(full_join) %>%
  ungroup() %>%
  mutate_at(vars(matches("pct_mass|ratio_mass|per_area|thickness")), ~if_else(. < 0, NA_real_, .)) %>%
  mutate_at(
    vars(matches("pct_mass")),
    ~add_metadata(., data_unit = "%")
  ) %>%
  add_column_metadata(
    leaf_dN15 = list(data_unit = ""),
    leaf_CN_ratio_mass = list(data_unit = ""),
    leaf_mass_per_area = list(data_unit = "g m-2"),
    leaf_water_thickness = list(data_unit = "g cm-2")
  ) %>%
  mutate(
    observation_id = paste(project_code, sample_name, year, sep = "|")
  )

species_info <- read_csv("extdata/species_dict/nasa_fft_species_dict.csv")

site_info <- meta_raw %>%
  distinct(site_code, Site_Name, State)

plot_info <- file.path(root_path, "Stand_Info", "Plot_Locations", "Aggregated_N_Coords_ALL.csv") %>%
  read_csv() %>%
  distinct(PLOT, SITE, LAT, LON) %>%
  rename(latitude = LAT, longitude = LON, site_code = SITE, plot_code = PLOT)

metadata <- meta2 %>%
  distinct(observation_id, .keep_all = TRUE) %>%
  full_join(traits) %>%
  left_join(species_info) %>%
  mutate(
    plot_code = ifelse(is.na(plot_code), str_extract(sample_name, "^[[:alnum:]]+"), plot_code),
    site_code = ifelse(is.na(site_code), str_extract(plot_code, "^[[:alpha:]]+"), site_code),
    is_experiment = FALSE
  ) %>%
  left_join(plot_info) %>%
  add_column_metadata(
    site_code = list(info = site_info)
  ) %>%
  add_metadata(!!!project_metadata)

create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)
