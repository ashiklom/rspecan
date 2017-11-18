rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_crop_N_LMA"
data_longname <- "Fresh leaf spectra to estimate leaf N concentration and leaf mass per area in two vegetable crops"
ecosis_file <- "raw_data/fresh-leaf-spectra-to-estimate-leaf-n-concentration-and-leaf-mass-per-area-in-two-vegetable-crops.csv"

message("Skipping dataset")
# Reason:
#   - Insufficient metadata
#   - Unclear which spectra correspond to what

#dat_full <- read_csv(ecosis_file) %>%
  #mutate(
    #spectra_id = sprintf("%s_%04d", data_name, row_number())
  #)

#wave_rxp <- "^[[:digit:]]+$"
#alt_rxp <- "^B([[:digit:]]+$)"
#spectra <- dat_full %>%
  #rename_at(vars(matches(alt_rxp)), ~gsub(alt_rxp, "\\1", .)) %>%
  #dat2specmat("spectra_id", wave_rxp)
#str(spectra)

#wl <- getwl(spectra)
#if (FALSE) {
  #matplot(wl, spectra, type = "l")
#}

#wl_prospect <- wl >= 400 & wl <= 2500
#wl_bad <- FALSE
#wl_keep <- wl_prospect & !wl_bad

#data_wl_inds <- which(wl_keep)
#wl_kept <- wl[wl_keep]
#prospect_wl_inds <- which(prospect_wl %in% wl_kept)

#stopifnot(length(data_wl_inds) == length(prospect_wl_inds))

#sp_good <- spectra[data_wl_inds, ]
#if (FALSE) {
  #matplot(wl_kept, sp_good, type = "l")
#}

#dat_sub <- dat_full %>%
  #select(-matches(wave_rxp), -matches(alt_rxp))

#dat_sub %>%
  #count(Field)
