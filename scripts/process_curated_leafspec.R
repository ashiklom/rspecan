rm(list = ls())
library(rspecan)
sethere()

data_name <- "curated_leafspec"
data_longname <- "Curated local leaf spectra database"

specdb <- src_sqlite("raw_data/leaf_spectra.db")

############################################################
# Process spectra
############################################################

message("Collecting spectra...")
spectra_wide <- tbl(specdb, "spectra_data") %>%
  collect() %>%
  spread(spectraid, spectravalue)

message("Converting to matrix...")
spectra <- spectra_wide %>%
  filter(!is.na(wavelength)) %>%
  select(-wavelength) %>%
  as.matrix()
rownames(spectra) <- as.character(spectra_wide$wavelength)
rm(spectra_wide); gc()
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
dat_sub <- tbl(specdb, "samples") %>%
  left_join(tbl(specdb, "spectra_info")) %>%
  left_join(tbl(specdb, "plots")) %>%
  left_join(tbl(specdb, "sites")) %>%
  collect()

dat <- dat_sub %>%
  transmute(
    data_name = !!data_name,
    spectra_id = as.character(spectraid),
    spectra_type = spectratype,
    USDA_code = speciescode,
    latitude = latitude,
    longitude = longitude
  ) %>%
  filter(
    !is.na(spectra_id),
    spectra_id %in% colnames(spectra)
  )

############################################################
# Store results
############################################################

store_path <- file.path(processed_dir, paste0(data_name, ".rds"))

datalist <- list(
  data_name = data_name,
  data_longname = data_longname,
  data_filename = "raw_data/leaf_spectra.db",
  self_filename = store_path,
  metadata = dat,
  spectra = spectra,
  data_wl_inds = data_wl_inds,
  prospect_wl_inds = prospect_wl_inds
)

check_datalist(datalist)

submit_df <- dat %>%
  filter(spectra_type %in% c("reflectance", "pseudo-absorbance")) %>%
  select(data_name, spectra_id)

saveRDS(datalist, store_path)
write_submit_file(submit_df, data_name)
