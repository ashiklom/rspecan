library(rspecan)
library(tidyverse)
library(PEcAnRTM)
library(metar)
library(data.table)
library(dtplyr)

import::from("lubridate", "ymd", "year")
import::from("udunits2", "ud.convert")

config_file <- here::here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

accp_path <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/ACCP/accp"

project_code <- "accp"
project_metadata <- list(
  project_code = project_code,
  short_name = "ACCP",
  long_name = "Accelerated Canopy Chemistry Program (ACCP)",
  spectra_methods = list(
    comment = "Unknown"
  )
)

#traits_path <- file.path(accp_path, "leafchem")
#traits_raw <- list.files(traits_path, "chem.dat$") %>%
  #map(~read_csv(file.path(traits_path, .), skip = 1)) %>%
  #bind_rows()

#traits2 <- traits_raw %>%
  #rename(
    #site_code = site_id,
    #species_code = species,
    #leaf_cellulose_pct_mass = cell,
    #leaf_lignin_pct_mass = lignin,
    #leaf_C_pct_mass = carbon,
    #leaf_H_pct_mass = hydrogen,
    #leaf_N_pct_mass = nitrogen,
    #leaf_chla_mass = chloro_a,
    #leaf_chlb_mass = chloro_b,
    #leaf_nonpolar_pct_mass = npolar,
    #leaf_polar_pct_mass = polar,
    #leaf_water_thickness = water_th,
    #leaf_lutein_mass = lutein,
    #leaf_neoxanthin = neoxanth,
    #leaf_betacarotene_mass = beta_car
  #) %>%
  #mutate(
    #observation_id = paste(project_code, sampleid),
    #plot_code = paste(site_code, plot_id, sep = "_"),
    #collection_date = ymd(colldate)
  #)

############################################################

# Load all chemistry data
traits_path <- file.path(accp_path, "leafchem")
traits_files <- list.files(traits_path, "chem.dat$")
traits_list <- list()
for (f in traits_files){
    print(f)
    fname <- file.path(traits_path, f)
    dat <- fread(fname, header=TRUE)
    traits_list[[f]] <- dat
}
traits_dat_raw <- rbindlist(traits_list, fill=TRUE)
colnames_dict <- c('sampleid' = 'samplename',
                   'species' = 'speciescode',
                   'cell' = 'leaf_cellulose_pct_mass',
                   'lignin' = 'leaf_lignin_pct_mass',
                   'carbon' = 'leaf_C_pct_mass',
                   'hydrogen' = 'leaf_H_pct_mass',
                   'nitrogen' = 'leaf_N_pct_mass',
                   'chloro_a' = 'leaf_chla_mass',
                   'chloro_b' = 'leaf_chlb_mass',
                   'npolar' = 'leaf_nonpolar_pct_mass',
                   'polar' = 'leaf_polar_pct_mass',
                   'water_th' = 'leaf_water_thickness',
                   'lutein' = 'leaf_lutein_mass',
                   'neoxanth' = 'leaf_neoxanthin_mass',
                   'beta_car' = 'leaf_betacarotene_mass')
setnames(traits_dat_raw, names(colnames_dict), colnames_dict)

pct_cols <- grep('_pct_', colnames_dict, value = TRUE)
mass_cols <- grep('[^p][^c][^t]_mass$', colnames_dict, value = TRUE)
new_mass_cols <- gsub('_mass$', '_pct_mass', mass_cols)

replace.na <- function(x) {
  if (!is.numeric(x)) x
  na_ind <- grepl("-9+$", as.character(x))
  x[na_ind] <- NA
  x
}

# Perform unit conversions
traits_dat_raw <- traits_dat_raw[, lapply(.SD, replace.na)] %>%
    .[, (mass_cols) := lapply(.SD, ud.convert, 'mg g-1', 'percent'), .SDcols = mass_cols] %>%
    setnames(mass_cols, new_mass_cols)

# Load all spectral data
spec_path <- file.path(accp_path, "leafspec")
spec_files <- list.files(spec_path, "sp.dat$")
refl_list <- list()
dat_list <- list()
for (f in spec_files) {
    if (grepl("_d_sp.dat", f)){
        prep <- "dry"
    } else if (grepl("_f_sp.dat", f)) {
        prep <- "fresh"
    } else if (grepl("_p_sp.dat", f)) {
        prep <- "powder"
    } else {
        warning("Unknown spectra type")
        next
    }
    if (grepl("jr_f_sp.dat", f)) {
        spectratype <- "reflectance"
    } else {
        spectratype <- "pseudo-absorbance"
    }
    fname <- file.path(spec_path, f)
    rawdat <- fread(fname)
    dat <- rawdat %>% select(-band)
    dat_melt <- melt(dat, id.vars = "wavelength",
                     variable.name = "samplename") %>%
        mutate(sampleprep = prep,
               fname = f,
               spectratype = spectratype) %>%
        filter(value > -0.5)    # NA values stored as -1
    dat_list[[f]] <- dat_melt
}
accp_spec <- rbindlist(dat_list)
rm(dat_list, dat_melt, dat)

# Revise bad sample codes
accp_spec <- accp_spec[, samplename := gsub("BH", "BHI", samplename)] %>%
    filter(!(samplename %in% c("92CWS61BA2A", "92CWS7FRN", "92HFS19RO4"))) %>%
    .[grepl("^(map|df)_", fname), samplename := paste0(samplename, "_", sampleprep)]

# Buld whole samples table
all_samples <- accp_spec %>%
    distinct(samplename, sampleprep, spectratype, fname) %>%
    full_join(traits_dat_raw) %>%
    .[, plot_id := as.character(plot_id)] %>%
    .[grepl("^map_", fname), 
      `:=`(site_id = "SaplingACMA3",
           speciescode = "ACMA3",
           plot_id = NA)] %>%
    .[grepl("^df_", fname), 
      `:=`(site_id = "SaplingPSME", 
           speciescode = "PSME",
           plot_id = NA)] %>%
# Populate additional metadata
    .[, collectiondate := as.POSIXct(as.character(colldate), format = "%y%m%d")] %>%
    .[, year := year(collectiondate)] %>%
    .[, projectcode := project_code] %>%
    .[, fullname := paste(project_code, samplename, year, sep = "|")] %>%
# `species.code` is already the correct USDA code
    .[, sitecode := site_id] %>%
    .[, plotcode := paste(sitecode, plot_id, sep = ".")] %>%
    .[speciescode == "USNEA", speciescode := "USNEA2"] %>%
    .[speciescode == "POGR", speciescode := "POSE"]

#species <- tbl(specdb, 'species') %>%
    #select(speciescode) %>%
    #collect() %>%
    #setDT()
#sp <- anti_join(all_samples %>% select(speciescode), species)

## Create sites table
accp_site_info <- tribble(
    ~sitecode, ~sitedescription, ~latitude, ~longitude, ~site_merge_tag,
    'HF', 'Harvard Forest, Petersham, MA', 42.4950, -71.7981, 'leaf',
    'BHI', 'Blackhawk Island, WI', 43.6333, -89.7583, 'leaf',
    'HOW', 'Howland, ME', 45.2222, -68.7356, 'leaf',
    'GAIN', 'Gainesville, FL', 29.7000, -82.1667, 'leaf',
    'JR', 'Jasper Ridge, CA', 37.4111, -121.7631, 'leaf',
    'RICE', 'Dunnigan and Pleasant Grove, CA', mean(c(38.9167, 38.7292)), mean(c(-120.1122, -120.4581)), 'leaf') %>%
    setDT()

site <- all_samples %>%
    distinct(projectcode, sitecode) %>%
    left_join(accp_site_info)

# Create plots table
accp_plots <- all_samples %>%
    distinct(plotcode, sitecode) %>%
    left_join(accp_site_info)

# Samples table
samples <- all_samples %>%
    distinct(fullname, projectcode, year, collectiondate,
             plotcode, speciescode) %>%
    rename(samplecode = fullname)

# Instrument
specmethods <- tribble(
    ~instrumentcode, ~specmethodcode, ~specmethodcomment, ~site_merge_tag,
    'nirs-6500', 'accp-leaf', 'ACCP measurement of wet and dry leaves', 'leaf',
    'accp-spec', 'accp-seedling', 'ACCP measurement of seedlings in field', 'seedling') %>%
    left_join(accp_site_info)

# Spec_info table
specinfo <- all_samples %>%
    filter(!is.na(fname)) %>%
    left_join(setDT(specmethods)) %>%
    select(samplecode = fullname, spectratype, sampleprep, specmethodcode)

# Spectra table
specdata <- accp_spec %>%
    left_join(all_samples %>% select(samplecode = fullname, samplename)) %>%
    select(samplecode, wavelength, spectravalue = value) %>%
    left_join(specinfo)

############################################################
meta_all <- all_samples %>%
  left_join(accp_site_info) %>%
  left_join(specmethods) %>%
  setDF() %>%
  as_tibble()

metadata <- meta_all %>%
  rename(
    observation_id = fullname,
    sample_prep = sampleprep,
    site_code = sitecode,
    plot_code = plotcode,
    species_code = speciescode,
    collection_date = collectiondate,
    instrument_code = instrumentcode
  ) %>%
  mutate(
    is_experiment = FALSE
  ) %>%
  mutate_at(
    vars(ends_with("pct_mass")),
    ~add_metadata(., data_unit = "%")
  ) %>%
  add_column_metadata(
    leaf_water_thickness = list(data_unit = "g cm-2")
  ) %>%
  select(
    -sitedescription, -site_merge_tag, -fname, -colldate,
    -specmethodcode, -specmethodcomment, -projectcode,
    -freshwgt, -dry_wgt, -leaf_wgt
  ) %>%
  add_metadata(!!!project_metadata)

spec_info <- specdata %>% distinct(samplecode, spectratype)

spec_df <- specdata %>%
  select(-spectratype, -sampleprep, -specmethodcode) %>%
  spread(samplecode, spectravalue)
waves <- spec_df$wavelength
spec_mat <- spec_df %>%
  select(-wavelength) %>%
  as.matrix()

spec_types <- spec_info %>%
  arrange(match(samplecode, colnames(spec_mat))) %>%
  pull(spectratype) %>%
  recode(`pseudo-absorbance` = "PA", `reflectance` = "R")

spectra <- spectra(spec_mat, waves, spec_types)

create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = metadata,
  overwrite = TRUE
)
