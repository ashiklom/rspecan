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

message("Fixing spectra that are in wrong units...")
mns <- colMeans(spectra, na.rm = TRUE)
gt1 <- mns > 1
gt1_ids <- colnames(spectra)[gt1]
spectra[, gt1] <- spectra[, gt1] / 100

lt1 <- mns < 0.01
lt1_ids <- colnames(spectra)[lt1]
spectra[, lt1] <- spectra[, lt1] * 100

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
apply_unit <- function(column) {
  colname <- deparse(substitute(column))
  col_unit <- gsub("^[^[:space:]]+[[:space:]]", "", colname)
  if (col_unit %in% c("unitless", "NA")) {
    units::set_units(column, units::unitless)
  } else {
    units::set_units(column, col_unit)
  }
}

strip_unit <- function(column) {
  strsplit(column, " ") %>%
    map(1) %>%
    unlist()
}

trait_dat <- tbl(specdb, "trait_data") %>%
  left_join(tbl(specdb, "trait_info")) %>%
  select(samplecode, trait, traitvalue, unit) %>%
  collect() %>%
  unite("trait_unit", trait, unit, sep = " ") %>%
  spread(trait_unit, traitvalue) %>%
  mutate_at(vars(-samplecode), apply_unit) %>%
  rename_at(vars(-samplecode), strip_unit)

conditions <- tbl(specdb, "sample_condition") %>%
  left_join(tbl(specdb, "sample_condition_info")) %>%
  select(samplecode, condition, conditionvalue) %>%
  collect() %>%
  spread(condition, conditionvalue) %>%
  mutate(
    canopy_position = case_when(
      !is.na(canopyposition) ~ canopyposition,
      !is.na(CanopyPosition) ~ CanopyPosition,
      TRUE ~ NA_character_
    )
  ) %>%
  select(-canopyposition, -CanopyPosition) %>%
  rename(sun_shade = sunshade)

dat_sub <- tbl(specdb, "samples") %>%
  left_join(tbl(specdb, "spectra_info")) %>%
  left_join(tbl(specdb, "plots")) %>%
  left_join(tbl(specdb, "sites")) %>%
  collect() %>%
  left_join(trait_dat) %>%
  left_join(conditions)

dat <- dat_sub %>%
  select(
    spectra_id = spectraid,
    spectra_type = spectratype,
    collection_date = collectiondate,
    USDA_code = speciescode,
    projectcode,
    latitude,
    longitude,
    colnames(trait_dat),
    colnames(conditions),
    -samplecode
  ) %>%
  rename(leaf_CN_ratio = leaf_CN_ratio_mass) %>%
  mutate(
    collection_date = lubridate::ymd(collection_date),
    data_name = !!data_name,
    spectra_id = as.character(spectra_id)
  ) %>%
  filter(
    !is.na(spectra_id),
    spectra_id %in% colnames(spectra)
  )

#dat %>%
  #filter(spectra_type == "continuum-removed reflectance") %>%
  #select(data_name, spectra_id) %>%
  #write.table(file = "submit_scripts/foster.submit",
              #sep = ",", row.names = FALSE)

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
