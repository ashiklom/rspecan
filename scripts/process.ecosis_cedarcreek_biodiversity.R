rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_cedarcreek_biodiversity"
data_longname <- "2014 Cedar Creek ESR Grassland Biodiversity Experiment: Leaf-level Contact Data: Trait Predictions"
ecosis_file <- "raw_data/2014-cedar-creek-esr-grassland-biodiversity-experiment--leaf-level-contact-data--trait-predictions.csv"

dat_full <- read_csv(ecosis_file) %>%
  mutate(spectra_id = sprintf("%s_%03d", data_name, ID))

############################################################
# Process spectra
############################################################
wave_rxp <- "^[[:digit:]]+$"
spectra_colname <- "spectra_id"

spectra <- dat2specmat(dat_full, spectra_colname, wave_rxp)
str(spectra)

wl <- getwl(spectra)
if (FALSE) {
  matplot(wl, spectra, type = "l")
}

wl_prospect <- wl >= 400 & wl <= 2500
wl_bad <- FALSE
wl_keep <- wl_prospect & !wl_bad

data_wl_inds <- which(wl_keep)
wl_kept <- wl[wl_keep]
prospect_wl_inds <- which(prospect_wl %in% wl_kept)

############################################################
# Process metadata
############################################################
dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    data_name = !!data_name,
    spectra_id = spectra_id,
    instrument = paste(`Instrument Manufacturer`, `Instrument Model`),
    spectra_type = `Measurement Quantity`,
    LMA = `LMA g m2`,
    LMA_unit = "g m-2",
    Cmass = `C`,
    Cmass_unit = "%",
    Nmass = N,
    Nmass_unit = "%",
    CN_ratio = `C:N`,
    lignin = Lignin,
    lignin_unit = "%",
    cellulose = `Cell`,
    cellulose_unit = "%",
    fiber = Fiber,
    fiber_unit = "%",
    chlorophyll = `Chl g m2`,
    chlorophyll_unit = "g m2",
    USDA_code = toupper(`USDA Symbol`),
    latitude = 45.402,
    longitude = -93.199
  )

############################################################
# Store results
############################################################

store_path <- file.path(processed_dir, paste0(data_name, ".rds"))

datalist <- list(
  data_name = data_name,
  data_longname = data_longname,
  data_filename = ecosis_file,
  self_filename = store_path,
  metadata = dat,
  spectra = spectra,
  data_wl_inds = data_wl_inds,
  prospect_wl_inds = prospect_wl_inds
)

check_datalist(datalist)

submit_df <- dat %>%
  filter(spectra_type == "reflectance") %>%
  select(data_name, spectra_id)

saveRDS(datalist, store_path)
write_submit_file(submit_df, data_name)
