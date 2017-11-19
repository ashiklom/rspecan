rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_cornvarieties"
data_longname <- "Spectral Characterization of Multiple Corn Varieties: West Madison Agricultural Station 2014"
ecosis_id <- "c0e238ea-5b23-452c-bc40-f0cfe2c6f032"
ecosis_file <- sprintf(
  "https://ecosis.org/package/export?package_id=%s&filters=&metadata=true",
  ecosis_id
)

message("Downloading data...")
dat_raw <- read_csv(ecosis_file)
message("Download complete!")

dat_full <- dat_raw %>%
  mutate(spectra_id = sprintf("%s_%03d_%02d", data_name, ID, rep))

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
    spectra_type = "reflectance",
    USDA_code = "ZEMA",
    variety = Variety,
    latitude = 43.0617,
    longitude = -89.532,
    instrument = "ASD FieldSpec 3"
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
