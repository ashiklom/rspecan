library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)

config_file <- here::here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

project_code <- "nasa_hyspiri"
project_metadata <- list(
  project_code = project_code,
  short_name = "NASA HyspIRI",
  long_name = "NASA HyspIRI field campaign",
  spectra_methods = list(
    comment = "???"
  )
)

siteinfo_path <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/NASA_HyspIRI/NASA_HyspIRI_California_Project_GPS_Locations_v2.xlsx'
rdata_path <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/NASA_HyspIRI/NASA_HyspIRI_Compiled_Field_Data.RData"
load(rdata_path)

raw_spectra <- as_data_frame(hyspiri_leaf_spectra) %>% 
    mutate(Spectra = as.character(Spectra),
           specnum = row_number())
raw_sla <- as_data_frame(SLA_and_Spectra) %>% 
    mutate(Spectra = as.character(Spectra),
           Sample_Name = as.character(Sample_Name))
raw_chn <- as_data_frame(chn.data) %>% 
    rename(species_code = USDA_Species_Code) %>% 
    mutate(collection_date = lubridate::mdy(Sample_Date))

# Match up spectra
sla_samples_raw <- raw_sla %>% 
    select(-starts_with('Wave')) %>% 
    mutate(collection_date = lubridate::ymd(Measurement_Date),
           # Fix mislabeled instrument
           Instrument = if_else(str_detect(Spectra, 'SJJR_Tower_PICO3_ELKCP4') & Instrument == 'FS4',
                                'FS3', as.character(Instrument)),
           instrument_code = fct_recode(Instrument,
                                       `asd-fs3` = 'FS3',
                                       `asd-fs4` = 'FS4',
                                       `se-psm3500` = 'SE')) %>%
    select(-Measurement_Date) %>% 
    # Average out duplicate measurements
    group_by(Sample_Name, Spectra, collection_date, instrument_code) %>%
    summarize_if(is.numeric, mean) %>%
    ungroup()

samples_raw <- raw_spectra %>% 
    select(-starts_with('Wave', ignore.case = FALSE)) %>% 
    mutate(collection_date = lubridate::ymd(Sample_Date),
           year = lubridate::year(collection_date),
           instrument_code = fct_recode(Instrument,
                                       `asd-fs3` = 'ASD_FS3',
                                       `asd-fs4` = 'ASD_FS4',
                                       `se-psm3500` = 'SpecEvo_PSM3500')
           ) %>% 
    select(-Sample_Date, -Instrument)

# This should be empty. That indicates that there are no SLA samples not in spectra
#anti_join(sla_samples_raw, samples_raw)

specstr <- samples_raw[['Spectra']]
speclist <- str_split(specstr, '_')
spec_length <- sapply(speclist, length)

table(spec_length)

sp4 <- do.call(rbind, speclist[spec_length == 4]) %>% 
    as_data_frame %>% 
    mutate(Spectra = specstr[spec_length == 4],
           site = V1,
           plot = V2,
           species = case_when(str_detect(.$V2, "OakMistletoe") ~ 'PHLE14',
                               TRUE ~ NA_character_),
           spectratype = case_when(str_detect(.$V4, '^[Rr]') ~ 'R',
                                   str_detect(.$V4, '^[Tt]') ~ 'T',
                                   TRUE ~ NA_character_),
           specnum = samples_raw$specnum[spec_length == 4],
           Sample_Name = paste(V1, V2, sep = '_')
           )

sp5 <- do.call(rbind, speclist[spec_length == 5]) %>%
    as_data_frame %>%
    mutate(Spectra = specstr[spec_length == 5],
           site = V1,
           plot = str_extract(Spectra, 'Plot\\d+'),
           specnum = samples_raw$specnum[spec_length == 5],
           species = case_when(.$V2 %in% c('Grape', 'CADE27', 'QUDO') ~ .$V2,
                               .$V3 == 'Grape' ~ 'Grape',
                               TRUE ~ NA_character_),
           spectratype = case_when(str_detect(.$V5, '^[Rr]') ~ 'R',
                                   str_detect(.$V5, '^[Tt]') ~ 'T',
                                   TRUE ~ NA_character_),
           Sample_Name = paste(V1, V2, V3, sep = '_'))

sp6 <- do.call(rbind, speclist[spec_length == 6]) %>%
    as_data_frame %>%
    mutate(Spectra = specstr[spec_length == 6],
           site = V1,
           specnum = samples_raw$specnum[spec_length == 6],
           species = case_when(.$V2 == 'Grape' ~ 'Grape',
                               !str_detect(.$V3, 'Bot|Top|Chem') ~ .$V3,
                               .$V2 == 'OrangeTree1' ~ 'OrangeTree',
                               TRUE ~ NA_character_),
           spectratype = case_when(str_detect(.$V6, '^[Rr]') ~ 'R',
                                   str_detect(.$V6, '^[Tt]') ~ 'T',
                                   TRUE ~ NA_character_),
           plot = V2,
           canopy_position = str_extract(V4, '(?<=^L\\d{1,2})(T|M|B)'),
           Sample_Name = paste(V1, V2, V3, V4, sep = '_'))

sp7 <- do.call(rbind, speclist[spec_length == 7]) %>%
    as_data_frame %>%
    mutate(Spectra = specstr[spec_length == 7],
           site = V1,
           plot = V2,
           species = V3,
           specnum = samples_raw$specnum[spec_length == 7],
           spectratype = case_when(str_detect(.$V6, '^[Rr]') ~ 'R',
                                   str_detect(.$V6, '^[Tt]') ~ 'T',
                                   TRUE ~ NA_character_),
           canopy_position = str_extract(V4, '(?<=^L\\d{1,2})(T|M|B)'),
           Sample_Name = paste(V1, V2, V3, V4, sep = '_'))

spectra_meta <- bind_rows(sp4, sp5, sp6, sp7) %>% 
    select(-matches('^V\\d+$')) %>%
    # Remove non-plant spectra
    filter(!species %in% c('BareField', 'LitterNeedles', 'NPV', 'Soil',
                           'CWD1', 'CWD2', NA)) %>% 
    mutate(species_code = recode(species,
                                `ImmatureOat` = 'AVENA',
                                `ImmatureOats` = 'AVENA',
                                `MatureOat` = 'AVENA',
                                `Almond` = 'PRDU',
                                `CAAN4sweet` = 'CAAN4',
                                `CAAN` = 'CAAN4',
                                `MandarinOrange` = 'CIRE3',
                                `Orange` = 'CITRU2',
                                `OrangeTree` = 'CITRU2',
                                `Peach` = 'PRPE3',
                                `LemonTree` = 'CILI5',
                                `LemonTrees` = 'CILI5',
                                `Grape` = 'VITIS',
                                `OakMistletoe` = 'PHLE14'),
           plot = case_when(is.na(.$plot) ~ NA_character_,
                            .$plot == 'na' ~ NA_character_,
                            .$plot == 'Grape' ~ NA_character_,
                            str_detect(.$plot, 'Chem\\d+') ~ NA_character_,
                            .$plot == 'TOWER' ~ 'Tower',
                            TRUE ~ .$plot)) %>%
    glimpse()

# TODO: Parse out exact coordinates
siteinfo <- readxl::read_excel(siteinfo_path) %>%
    group_by(Site) %>%
    summarize(latitude = mean(Latitude, na.rm = TRUE),
              longitude = mean(Longitude, na.rm = TRUE)) %>%
    mutate(site_code = recode(Site,
                             `Kingsburg` = 'KING',
                             `Shafter` = 'SHAF',
                             `Russell Ranch` = 'RR')) %>%
    ungroup() %>%
    select(-Site)


# TODO: Identify individual leaves
samples <- samples_raw %>%
    inner_join(spectra_meta) %>%
    filter(!is.na(Sample_Name)) %>%
    rename(site_code = site) %>%
    mutate(project_code = project_code,
           observation_id = paste(project_code,
                                  paste(Sample_Name, collection_date, instrument_code, sep = '_'),
                                  year,
                                  sep = '|'),
           plotcode = paste(site_code, plot, sep = '.'),
           sunshade = recode(canopy_position,
                             `B` = 'shade',
                             `M` = 'shade',
                             `T` = 'sun',
                             .missing = NA_character_)) %>%
    left_join(siteinfo)
    
## TODO: Fill in spec methods
#specmethods <- tribble(
#    ~spectratype, ~apparatus, 
#    'reflectance', 'Leaf clip',
#    'transmittance', 'Integrating sphere') %>%
#    mutate(instrumentname = 

spec_obs <- samples %>%
  distinct(Spectra, observation_id, spectratype)

spec_df <- raw_spectra %>%
  left_join(spec_obs) %>%
  select(observation_id, spectratype, starts_with("Wave_")) %>%
  filter(!is.na(observation_id), !is.na(spectratype))

obs_ids <- spec_df$observation_id
st <- spec_df$spectratype
waves <- spec_df %>%
  select(-observation_id, -spectratype) %>%
  colnames() %>%
  str_remove("^Wave_") %>%
  as.numeric()

message("Processing spectra")
spectra <- spec_df %>%
  select(-observation_id, -spectratype) %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(obs_ids) %>%
  spectra(waves, st)
spectra[spectra < 0 | spectra > 1] <- NA
spectra <- spectra[, spectra[[1000]] > 0.1]

# trait units:
# leaf_mass_per_area -- g m-2
# SLA -- cm2 g-1
# *_pct_mass -- %

message("Processing traits")
sla_averaged <- sla_samples_raw %>%
    inner_join(samples %>% distinct(observation_id, Sample_Name, collection_date)) %>%
    group_by(observation_id) %>%
    summarize(leaf_mass_per_area = mean(LMA_gDW_m2, na.rm = TRUE)) %>%
    ungroup()

trait_data <- raw_chn %>%
    mutate(Sample_Name = str_replace(Sample_Name, '_LC(_RG)?$', ''),
           Sample_Name = if_else(Sample_Name == 'SCRec_Plot9_PEAM3_ELK6T6sun2',
                                 paste0(Sample_Name, 'time2'), Sample_Name),
           Sample_Name = str_replace(Sample_Name, 'TOWER', 'Tower')) %>%
    select(Sample_Name, collection_date,
           leaf_C_pct_mass = Perc_C,
           leaf_H_pct_mass = Perc_H,
           leaf_N_pct_mass = Perc_N) %>%
    mutate(leaf_CN_ratio = leaf_C_pct_mass / leaf_N_pct_mass) %>%
    inner_join(samples %>% distinct(observation_id, Sample_Name, collection_date)) %>%
    # Keep only unambiguous matches
    group_by(Sample_Name, collection_date) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n == 1) %>%
    select(observation_id, starts_with('leaf_', ignore.case = FALSE)) %>%
    full_join(sla_averaged) %>%
    group_by(observation_id) %>%
    summarize_all(mean, na.rm = TRUE)

message("Finalizing metadata")
metadata <- samples %>%
  left_join(trait_data) %>%
  select(observation_id, everything()) %>%
  select(
    -Site, -specnum, -species, -spectratype,
    -Sample_Name, -Spectra, -plot
  ) %>%
  distinct() %>%
  rename(
    plot_code = plotcode,
    sun_shade = sunshade
  ) %>%
  mutate(
    sun_shade = factor(sun_shade, c("sun", "shade")),
    canopy_position = factor(canopy_position, c("B", "M", "T")),
    is_experiment = FALSE,
    Database_ID = "USDA Plants"
  ) %>%
  mutate_at(
    vars(ends_with("_pct_mass")),
    ~add_metadata(., data_unit = "%")
  ) %>%
  add_column_metadata(
    leaf_mass_per_area = list(data_unit = "g m-2"),
    leaf_CN_ratio = list(data_unit = "")
  ) %>%
  add_metadata(!!!project_metadata) %>%
  glimpse()

#metadata %>%
  #count(observation_id) %>%
  #filter(n > 1) %>%
  #semi_join(metadata, .) %>%
  #filter(observation_id == first(observation_id)) %>%
  #glimpse()

message("Creating project")
create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)
message("Done!")
