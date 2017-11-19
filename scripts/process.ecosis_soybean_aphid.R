rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_soybean_aphid"
data_longname <- "Productivity and Characterization of Soybean Foliar Traits Under Aphid Pressure"
ecosis_id <- "cdbb6b09-b481-4022-a0da-ad95a8b085d8"
ecosis_file <- sprintf(
  "https://ecosis.org/package/export?package_id=%s&metadata=true",
  ecosis_id
)

message("Downloading data...")
dat_raw <- read_csv(ecosis_file)
message("Download complete!")

dat_full <- dat_raw %>%
  mutate(spectra_id = sprintf("%s_%03d", data_name, SAMP_ID))

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
    carotenoids = CAROTENOIDS,
    carotenoids_unit = "ug cm-2",
    chlorophyll_a = CHL_a,
    chlorophyll_a_unit = "ug cm-2",
    chlorophyll_b = CHL_b,
    chlorophyll_b_unit = "ug cm-2",
    chlorophyll_total = chlorophyll_a + chlorophyll_b,
    chlorophyll_total_unit = "ug cm-2",
    collection_date = ISOdate(YYYY, MM, DD),
    instrument = INSTRUMENT,
    latitude = LATITUDE,
    longitude = LONGITUDE,
    sun_shade = recode(LEAF_HEIGHT, L = "shade", U = "sun"),
    soy_stage = STAGE,
    treatment_soy = TREAT,
    LMA = gmm2_LMA,
    LMA_unit = "g m-2",
    Cmass = pct_CARBON,
    Cmass_unit = "%",
    cellulose = pct_CELLULOSE,
    cellulose_unit = "%",
    fiber = pct_FIBER,
    fiber_unit = "%",
    lignin = pct_LIGNIN,
    lignin_unit = "%",
    Nmass = pct_NITROGEN,
    Nmass_unit = "%",
    is_agriculture = TRUE
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
