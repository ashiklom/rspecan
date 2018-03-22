rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_serbin_ntforests"
data_longname <- "Fresh Leaf Spectra to Estimate Leaf Morphology and Biochemistry for Northern Temperate Forests"
ecosis_id <- "4a63d7ed-4c1e-40a7-8c88-ea0deea10072"
ecosis_file <- sprintf(
  "https://ecosis.org/package/export?package_id=%s&metadata=true",
  ecosis_id
)

stop("This is Shawn's FFT data. Just use my local copy instead.")

message("Downloading data...")
dat_raw <- read_csv(ecosis_file)
message("Download complete!")
dat_full <- dat_raw %>%
  group_by(Spectra) %>%
  mutate(spectra_id = sprintf("%s_%d", Spectra, row_number())) %>%
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

spectra[spectra < 0] <- NA

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

species_dict <- read_csv("raw_data/ancillary/nasa_fft_species_dict.csv")
plot_coords <- read_csv("raw_data/ancillary/nasa_fft_plot_coords.csv") %>%
  distinct(SITE, PLOT, LAT, LON)

dat <- dat_sub %>%
  left_join(species_dict, by = c("Species" = "speciesdatacode")) %>%
  left_join(plot_coords, by = c("Plot" = "PLOT", "Site" = "SITE")) %>%
  transmute(
    data_name = !!data_name,
    spectra_id = spectra_id,
    spectra_type = measurement,
    USDA_code = speciescode,
    latitude = LAT,
    longitude = LON,
    needle_age = Age,
    canopy_position = recode(tolower(Height), "b" = "bottom", "m" = "middle", "t" = "top")
  )

glimpse(dat)
  transmute(
    data_name = !!data_name,
    spectra_id = spectra_id,
    spectra_type = measurement,
    USDA_code = speciescode,
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
