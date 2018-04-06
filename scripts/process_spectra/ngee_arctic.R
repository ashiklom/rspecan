library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)
library(readxl)
library(data.table)

config_file <- here::here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

project_code <- "ngee_arctic"

project_metadata <- list(
  project_code = project_code,
  short_name = "NGEE Arctic",
  long_name = "Next Generation Ecosystem Experiment (NGEE) - Arctic",
  site_description = "North Slope of Alaska, USA",
  spectra_methods = list(
    comment = "Ask Shawn Serbin"
  )
)

path_nga <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/NGEE-Arctic/"

# Load spectral data
speclist <- list()

spec_2013_file <- file.path(path_nga, "2013_data", "NGEE-Arctic_2013_Spectra_and_Trait_Data_QA_v2_forR.csv")
speclist[["2013"]] <- read_csv(spec_2013_file) %>%
  mutate(
    SampleName = paste0("BNL", Sample_ID),
    year = 2013,
    sitecode = "Barrow",
  )

spec_2014_file <- file.path(path_nga, "2014_Data", "NGEE-Arctic_Barrow_2014_Leaf_GasExchange_Spectra.xlsx")
speclist[["2014"]] <- read_excel(spec_2014_file, sheet = 1) %>% 
  rename(SampleName = Spectra) %>%
  mutate(year = 2014, sitecode = "Barrow")

spec_2015_file <- file.path(path_nga, "2015_Data", "NGEE-Arctic_Barrow_2015_SVC_Leaf_Spectra.xlsx")
speclist[["2015"]] <- read_excel(spec_2015_file, sheet = 1) %>%
  rename(SampleName = Sample_Barcode) %>%
  mutate(year = 2015, sitecode = "Barrow")

spec_2016b_file <- file.path(path_nga, "2016_Data", "NGEE-Arctic_Barrow_2016_SVC_Leaf_Spectra.xlsx")
speclist[["2016b"]] <- read_excel(spec_2016b_file, sheet = 1) %>%
  rename(SampleName = Sample_Barcode) %>%
  mutate(year = 2016, sitecode = 'Barrow')

spec_2016s_file <- file.path(path_nga, "2016_Data", "NGEE-Arctic_Seward_2016_HR1024i_Leaf_Spectral_Reflectance.xlsx")
speclist[["2016s"]] <- read_excel(spec_2016s_file, sheet = 2) %>%
  rename(SampleName = BNL_Barcode) %>%
  mutate(year = 2016, sitecode = 'Seward_Kougarok')

specdat_full <- bind_rows(speclist) %>%
  mutate(observation_id = paste(project_code, SampleName, year, sep = '|'))

samples_spec <- specdat_full %>%
  select(-starts_with("Wave_")) %>%
  group_by(Spectrometer_GPS_Lat, Spectrometer_GPS_Long) %>%
  mutate(
    plot_code = if_else(is.na(Spectrometer_GPS_Lat), NA_character_, as.character(row_number()))
  ) %>%
  ungroup() %>%
  select(
    observation_id,
    SampleName,
    year,
    site_code = sitecode,
    plot_code,
    latitude = Spectrometer_GPS_Lat,
    longitude = Spectrometer_GPS_Long
  ) %>%
  mutate(plot_code = paste(site_code, plot_code, sep = "."))

obs_ids <- specdat_full %>% pull(observation_id)
spec_raw1 <- specdat_full %>%
  select(starts_with("Wave_"))
waves <- colnames(spec_raw1) %>% str_remove("Wave_") %>% as.numeric()

spectra <- spec_raw1 %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, "R")
cmeans <- colMeans(spectra)
rescale <- cmeans > 1
spectra[, rescale] <- spectra[, rescale] * 0.01
spectra[spectra < 0] <- NA

# Load main traits file
traits_main_file <- file.path(path_nga, "NGEEArctic_BNL_leaf_C_N_LMA_2012-2015.xlsx")
traits_main <- read_excel(traits_main_file, sheet = 2) %>%
  .[, !is.na(colnames(.)) & colnames(.) != ""] %>%
  filter(!is.na(Sample_ID)) %>%
  mutate_if(is.numeric, na_if, y=-9999) %>%
  rename(
    SampleName = Sample_ID,
    speciesdatacode = USDA_Species_Code
  ) %>%
  mutate(
    collectiondate = as.POSIXct(strptime(Measurement_Date, "%Y%m%d")),
    year = lubridate::year(collectiondate),
    sitecode = "Barrow",
    leaf_mass_per_area = add_metadata(LMA_gDW_m2, data_unit = "g m-2"),
    leaf_N_per_area = add_metadata(N_area_gDW_m2, data_unit = "g m-2"),
    leaf_C_per_area = add_metadata(C_area_gDW_m2, data_unit = "g m-2"),
    leaf_C_pct_mass = add_metadata(Perc_C, data_unit = "%"),
    leaf_N_pct_mass = add_metadata(Perc_N, data_unit = "%"),
    leaf_CN_ratio_mass = add_metadata(CN_ratio, data_unit = "")
  ) %>%
  select(
    SampleName, year, collectiondate, sitecode, speciesdatacode,
    starts_with("leaf_")
  )

# Load pigment data for 2015
pigments_file <- file.path(path_nga, "2015_Data", "NGEE-Arctic_Barrow_2015_leaf_pigment_extractions.xlsx")
pigments <- read_excel(pigments_file, sheet = 2) %>%
  rename(SampleName = Barcode) %>%
  mutate_at(
    c("Chl_a_mg_m2", "Chl_b_mg_m2", "Chl_a_plus_b_mg_m2", "Tot_Car_mg_m2"),
    ~udunits2::ud.convert(., "mg m-2", "ug cm-2")
  ) %>%
  mutate(
    leaf_chla_per_area = add_metadata(Chl_a_mg_m2, data_unit = "ug cm-2"),
    leaf_chlb_per_area = add_metadata(Chl_b_mg_m2, data_unit = "ug cm-2"),
    leaf_chltot_per_area = add_metadata(Chl_a_plus_b_mg_m2, data_unit = "ug cm-2"),
    leaf_cartot_per_area = add_metadata(Tot_Car_mg_m2, data_unit = "ug cm-2"),
    leaf_area = add_metadata(Total_area_m2, data_unit = "m2"),
    year = 2015,
    sitecode = "Barrow"
  ) %>%
  select(SampleName, year, sitecode, starts_with("leaf"))

# Get other trait data from 2013
traits2013 <- speclist[["2013"]] %>%
  select(-starts_with('Wave')) %>%
  mutate_at(
    c("Chl_a_g_m2", "Chl_b_g_m2"),
    ~udunits2::ud.convert(., "g m-2", "ug cm-2")
  ) %>%
  mutate(
    leaf_chla_per_area = add_metadata(Chl_a_g_m2, data_unit = "ug cm-2"),
    leaf_chlb_per_area = add_metadata(Chl_b_g_m2, data_unit = "ug cm-2"),
    leaf_chltot_per_area = leaf_chla_per_area + leaf_chlb_per_area,
    leaf_N_per_area = add_metadata(Narea_gN_m2, data_unit = "g m-2"),
    leaf_mass_per_area = add_metadata(LMA_gDW_m2, data_unit = "g m-2"),
    leaf_area = add_metadata(Total_Leaf_Area_m2, data_unit = "m2")
  ) %>%
  select(
    SampleName, year, speciesdatacode = USDA_Species,
    starts_with("leaf_", ignore.case = FALSE)
  )

# Load other data from 2016
seward_2016_lma_file <- file.path(path_nga, "2016_Data", "2016KGsamples_LMA-edited.csv")
seward_2016_lma <- read_csv(seward_2016_lma_file) %>%
  rename(sitecode = Site) %>%
  mutate(
    SampleName = paste0("BNL", Sample_Barcode),
    leaf_mass_per_area = add_metadata(LMA_gDW_m2, data_unit = "g m-2"),
    year = 2016,
    collectiondate = as.POSIXct(strptime(Measurement_Date, "%Y%m%d"))
  ) %>%
  select(
    SampleName, sitecode, year, speciesdatacode = USDA_Species_Code,
    collectiondate, leaf_mass_per_area
  )

barrow_2016_chn_file <- file.path(path_nga, "2016_Data", "Barrow2016_CHN_analysis-edited.xlsx")
barrow_2016_chn <- read_excel(barrow_2016_chn_file, sheet = 2) %>%
  rename(
    SampleName = Sample_Barcode,
    speciesdatacode = USDA_Species_Code,
    leaf_C_pct_mass = Perc_C,
    leaf_N_pct_mass = Perc_N,
    leaf_CN_ratio_mass = CN_ratio
  ) %>%
  add_column_metadata(
    leaf_C_pct_mass = list(data_unit = "%"),
    leaf_N_pct_mass = list(data_unit = "%"),
    leaf_CN_ratio_mass = list(data_unit = "")
  ) %>%
  mutate(year = 2016, sitecode = "Barrow") %>%
  select(SampleName, speciesdatacode, sitecode, starts_with("leaf"))

barrow_2016_lma_file <- file.path(path_nga, "2016_Data", "Barrow2016_samples_LMA.csv")
barrow_2016_lma <- read_csv(barrow_2016_lma_file) %>%
  mutate(
    SampleName = paste0("BNL", Sample_Barcode),
    year = 2016,
    collectiondate = as.POSIXct(strptime(Measurement_Date, "%Y%m%d")),
    sitecode = "Barrow",
    leaf_mass_per_area = add_metadata(LMA_gDW_m2, data_unit = "g m-2")
  ) %>%
  select(
    SampleName, year, sitecode, collectiondate,
    speciesdatacode = Species, leaf_mass_per_area
  )

dat_2016 <- full_join(seward_2016_lma, barrow_2016_lma) %>%
  full_join(barrow_2016_chn)
dat_other <- full_join(traits_main, pigments) %>%
  filter(!is.na(year))

setDT(traits2013)

traits_full <- full_join(dat_2016, dat_other) %>%
  setDT() %>%
  setkey(SampleName) %>%
  .[traits2013[, SampleName],
    `:=`(leaf_chla_per_area = traits2013[, leaf_chla_per_area],
          leaf_chlb_per_area = traits2013[, leaf_chlb_per_area],
          leaf_chltot_per_area = traits2013[, leaf_chltot_per_area])] %>%
  setDF() %>%
  as_tibble() %>%
  rename(
    site_code = sitecode,
    species_data_code = speciesdatacode,
    collection_date = collectiondate
  ) %>%
  mutate(
    observation_id = paste(project_code, SampleName, year, sep = "|")
  )

samples_raw <- samples_spec %>%
  full_join(traits_full) %>%
  mutate(
    projectcode = project_code,
    observation_id = paste(projectcode, SampleName, year, sep = "|"),
    plot_code = if_else(is.na(plot_code), paste(site_code, plot_code, sep = "."), plot_code)
  ) %>%
  left_join(read_csv("extdata/species_dict/ngee_arctic_species_dict.csv"))

# TODO: Finer resolution for plot latitude and longitude...?
plots <- samples_raw %>%
    group_by(site_code, plot_code) %>%
    summarize(latitude = mean(latitude), longitude = mean(longitude)) %>%
    mutate(
      latitude = if_else(is.na(latitude) & site_code == "Barrow", 71.275764, latitude),
      longitude = if_else(is.na(longitude) & site_code == "Barrow", -156.641386, longitude)
    )

metadata <- samples_raw %>%
  select(-latitude, -longitude) %>%
  left_join(plots) %>%
  mutate(
    instrument_code = "unknown",
    is_experiment = FALSE
  ) %>%
  add_metadata(!!!project_metadata)

create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)

## Sanity checks
#samples %>% group_by(sitecode) %>% count()
#samples %>% group_by(speciescode) %>% count()
#samples %>% filter(is.na(speciescode)) %>% 
    #group_by(sitecode, year) %>% count()
