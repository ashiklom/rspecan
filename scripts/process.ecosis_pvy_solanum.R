rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_pvy_solanum"
data_longname <- "Varietal Discrimination and Detection of PVY in Solanum tuberosum: Hawaii 2014"
ecosis_file <- "raw_data/varietal-discrimination-and-detection-of-pvy-in-solanum-tuberosum--hawaii-2014.csv"

dat_full <- read_csv(ecosis_file) %>%
  mutate(
    spectra_id = sprintf("%s_%04d_%02d", data_name, ID, rep)
  )
wave_rxp <- "^[[:digit:]]+$"

spectra <- dat2specmat(dat_full, "spectra_id", wave_rxp)
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

sp_good <- spectra[data_wl_inds, ]
if (FALSE) {
  matplot(wl_kept, sp_good, type = "l")
}

dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    data_name = !!data_name,
    spectra_id = spectra_id,
    spectra_type = "reflectance",
    PVY_infected = recode(infect, h = "healthy", i = "infected"),
    variety = VAR,
    genus = "Solanum",
    species = "tuberosum",
    USDA_code = "SOTU"
  )

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
