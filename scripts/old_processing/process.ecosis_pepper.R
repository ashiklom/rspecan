rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_pepper"
data_longname <- "Fresh and Dry Pepper Leaf Spectra with Associated Potassium and Nitrogen Measurements"
ecosis_id <- "a67925bf-f715-449a-939c-3cb000fb7889"
ecosis_file <- sprintf(
  "https://ecosis.org/package/export?package_id=%s&metadata=true",
  ecosis_id
)

message("Downloading data...")
dat_raw <- read_csv(ecosis_file)
write_csv(dat_raw, "pepper_leaf_spectra_potassium_nitrogen.csv")
message("Download complete!")

dat_full <- dat_raw %>%
  group_by(PlantNumber) %>%
  mutate(spectra_id = sprintf("%s_%02d_%02d", data_name, PlantNumber, row_number())) %>%
  ungroup()

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
    USDA_code = "CAAN4",
    spectra_type = recode(
      Measurement_Type,
      "Fresh Transmittance" = "transmittance",
      "Fresh Reflectance" = "reflectance",
      "Fresh Absorbance" = "absorbance",
      "Dry Reflectance" = "reflectance"
    ),
    fresh_dry = case_when(
      grepl("^Fresh", Measurement_Type) ~ "fresh",
      grepl("^Dry", Measurement_Type) ~ "dry",
      TRUE ~ NA_character_
    ),
    leaf_N_pct_mass = units::set_units(`Leaf nitrogen content per leaf dry mass (% DW)`, "%"),
    leaf_K_pct_mass = units::set_units(`Leaf potassium content per leaf dry mass (% DW)`, "%")
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
