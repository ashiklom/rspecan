library(rspecan)
library(here)

if (!exists("overwrite")) overwrite <- FALSE

data_path <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/curated-leafspec/data/lopex"
project_code <- "lopex"
specdb_file <- "test.h5"

project_info <- list(
  short_name = "LOPEX",
  long_name = "Leaf Optical Properties Experiment (1993)",
  URL = "http://opticleaf.ipgp.fr/index.php?page=database"
)

common_metadata <- tibble(
  site_code = "ispra",
  plot_code = "ispra",
  site_description = "Joint Research Center, Ispra, Italy",
  latitude = 45.803,
  longitude = 8.630,
  year = 1993,
  instrument_code = "perkin-elmer-l19",
  apparatus = "integrating sphere",
  calibration = "spectralon ratio",
  method_comment = "See http://teledetection.ipgp.jussieu.fr/opticleaf/lopex.htm#spectral for more info",
  is_experiment = FALSE
)

# Set paths for LOPEX data
chem_path <- file.path(data_path, "LDB_lopex1993.csv")
spec_path <- file.path(data_path, "spec")

# Load main data.
species_info <- read_csv(here("extdata/species_dict/lopex_species_dict.csv"))
species_rxp <- "([[:alpha:]]{3})[[:alpha:]]* x? ?([[:alpha:]]{3})[[:alpha:]]* *.*"
lopex_raw <- read_csv(chem_path) %>%
  rename(raw_species_code = `Latin Name`) %>%
  select(-`Plant Type`, -`English Name`) %>%
  as_tibble() %>%
  mutate_if(is.numeric, na_if, y = -999) %>%
  fill(raw_species_code) %>%
  mutate(
    observation_id = sprintf("lopex_%02d", group_indices(., raw_species_code, C_a, C_b, C_car))
  ) %>%
  select(observation_id, everything())

lopex_traits <- lopex_raw %>%
  rename(
    leaf_C_pct_mass = C_C,
    leaf_H_pct_mass = C_H,
    leaf_O_pct_mass = C_O,
    leaf_N_pct_mass = C_N,
    leaf_prospect_N = N
  ) %>%
  mutate(
    leaf_chla_per_area = ud.convert(C_a, "ug cm-2", "kg m-2"),
    leaf_chlb_per_area = ud.convert(C_b, "ug cm-2", "kg m-2"),
    leaf_chltot_per_area = ud.convert(C_ab, "ug cm-2", "kg m-2"),
    leaf_cartot_per_area = ud.convert(C_car, "ug cm-2", "kg m-2"),
    leaf_anth_per_area = ud.convert(C_anth, "ug cm-2", "kg m-2"),
    leaf_mass_per_area = ud.convert(LMA, "g cm-2", "kg m-2"),
    leaf_water_thickness = ud.convert(EWT, "g cm-2", "kg m-2"),
    leaf_CN_ratio_mass = leaf_C_pct_mass / leaf_N_pct_mass,
    leaf_protein_pct_mass = (C_prot1 + C_prot2) / 2,
    leaf_cellulose_pct_mass = (C_cell1 + C_cell2) / 2,
    leaf_lignin_pct_mass = (C_lign1 + C_lign2) / 2,
    leaf_starch_pct_mass = case_when(
      !is.na(C_star1) & !is.na(C_star2) ~ (C_star1 + C_star2) / 2,
      !is.na(C_star1) ~ C_star1,
      !is.na(C_star2) ~ C_star2,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    -C_a, -C_b, -C_ab, -C_car, -C_anth, -LMA, -EWT,
    -C_prot1, -C_prot2, -C_cell1, -C_cell2, -C_lign1, -C_lign2,
    -C_star1, -C_star2
  )

species_info <- read_csv(here("extdata/species_dict/lopex_species_dict.csv"))
metadata_all <- lopex_traits %>%
  left_join(species_info) %>%
  add_column(!!!common_metadata)

dedup <- function(x) {
  if (is.numeric(x)) {
    out <- mean(x, na.rm = TRUE)
  } else {
    out <- unique(x)
  }
  stopifnot(length(out) == 1)
  out
}

metadata <- metadata_all %>%
  select(-Refl_file, -Trans_file) %>%
  group_by(observation_id) %>%
  summarize_all(dedup) %>%
  distinct()

read_spec <- function(files, spectra_types) {
  map(file.path(spec_path, files), data.table::fread) %>%
    map(~spectra(.[[2]], .[[1]], spectra_types = spectra_types)) %>%
    reduce(cbind)
}

lopex_spec <- metadata_all %>%
  select(observation_id, Refl_file, Trans_file) %>%
  mutate(
    reflectance = map(Refl_file, read_spec, spectra_types = "R"),
    transmittance = map(Trans_file, read_spec, spectra_types = "T"),
    spectra = map2(reflectance, transmittance, cbind),
    spectra = map2(spectra, observation_id, ~`colnames<-`(.x, rep(.y, 2)))
  ) %>%
  pull(spectra) %>%
  do.call(cbind, .)

create_project(
  specdb_file = specdb_file,
  project_code = project_code,
  project_info = project_info,
  spectra = lopex_spec,
  metadata = metadata,
  overwrite = overwrite
)
