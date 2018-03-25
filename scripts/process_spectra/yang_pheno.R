library(rspecan)
library(tidyverse)
library(metar)
library(PEcAnRTM)
import::from("readxl", "read_excel")

config_file <- here::here("scripts/process_spectra/config.R")
stopifnot(file.exists(config_file))
source(config_file)

project_code <- "yang_pheno"
project_metadata <- list(
  project_code = project_code,
  short_name = "Yang 2016",
  long_name = "Yang et al. 2016 Remote Sensing of Environment",
  doi = "10.1016/j.rse.2016.03.026",
  spectra_methods = list(
    comment = "TODO"
  )
)

# MV_MA (Martha's Vineyard) -- 41.362, -70.578
# A,B,C indicate three individuals
# U,L mean upper and lower
# N,S mean leaves were on the north and south part of the canopy
# 1,2,3 mean different leaves
# All trees are Red Oak

##' Sites table
sites <- tribble(
  ~site_code, ~description, ~latitude, ~longitude,
  "MV_MA", "Martha\'s Vineyard, MA, USA", 41.362, -70.578,
  "HF_MA", "Harvard Forest, Petersham, MA, USA", 42.531, -72.190
) %>%
  mutate(plot_code = site_code)

# HF_MA
# RO -- Red Oak
# RM -- Red maple
# YB -- Yellow Birch
# U,L are upper and lower 
# First number indicates trees
# Second number indicates leaves

# Set up trait names
traits <- c('leaf_chla_per_area' = 'Chla',
            'leaf_chlb_per_area' = 'Chlb',
            'leaf_chltot_per_area' = 'TotChl',
            'leaf_mass_per_area' = 'LMA',
            'leaf_cartot_per_area' = 'Car',
            'leaf_C_pct_mass' = 'TotC',
            'leaf_N_pct_mass' = 'TotN')
ntraits <- length(traits)

rootdir <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/Yang_etal'

readYang <- function(SampleYear, Site) {
  fname <- sprintf("%s/%d_%s_leaftraits_forShawn.xlsx",
                   rootdir, SampleYear, str_remove(Site, "_MA"))
  dat_list <- list()
  SampleNames <- character()
  is_MV <- SampleYear == 2011 & Site == "MV_MA"
  is_HF <- SampleYear == 2012 & Site == "HF_MA"

  trait_list <- traits %>%
    map(~read_excel(path = fname, sheet = .)) %>%
    map2(names(traits), ~mutate(.x, trait = !!.y)) %>%
    map(~rename(., DOY = !!names(.)[1]))

  trait_data <- trait_list %>%
    bind_rows() %>%
    gather("tree_id", "value", -DOY, -trait) %>%
    spread(trait, value) %>%
    mutate(
      year = SampleYear,
      site_code = Site,
      plot_code = site_code,
      observation_id = paste(project_code, tree_id, DOY, year, sep = "|"),
      is_experiment = FALSE
    ) %>%
    left_join(sites)

  species_cols <- "RawSpecies"
  species <- c("RO" = "Quercus rubra",
                "RM" = "Acer rubrum",
                "YB" = "Betula alleghaniensis")

  if (is_MV){
    trait_data[["species_data_code"]] <- "Quercus rubra"
  } else if (is_HF) {
    trait_data <- trait_data %>%
      mutate(
        label = gsub("(RO|RM|YB).*", "\\1", tree_id),
        species_data_code = species[label]
      ) %>%
      select(-label)
  }

  trait_data <- trait_data %>%
    mutate(
      sun_shade = case_when(
        grepl("U", tree_id) ~ "sun",
        grepl("L", tree_id) ~ "shade"
      ),
      sun_shade = factor(sun_shade, c("sun", "shade"))
    )

  metadata <- trait_data %>%
    #distinct(samplecode, projectcode, sitecode, plotcode, speciesdatacode, DOY, year) %>%
    mutate(
      collectiondate = as.Date(paste(year, DOY, sep = "_"), "%Y_%j")
    ) %>%
    left_join(read_csv("extdata/species_dict/yang_pheno_species_dict.csv"))

  # Load spectral data
  read_spec <- function(fname, doy) {
    obs_ids <- metadata %>% filter(DOY == doy) %>% pull(observation_id)
    read_csv(fname, col_names = FALSE) %>%
      `colnames<-`(c("wavelength", obs_ids)) %>%
      filter(wavelength != 0)
  }

    doys <- sort(metadata %>% distinct(DOY) %>% pull())
    if (is_MV) {
        reflpath <- file.path(rootdir, "2011-MV")
    } else if (is_HF) {
        reflpath <- file.path(rootdir, "2012_HF/ref")
        transpath <- file.path(rootdir, "2012_HF/tra")
    }

    refl_flist <- list.files(reflpath, ".csv", full.names = TRUE)
    stopifnot(length(refl_flist) == length(doys))

    spec_dat <- map2(refl_flist, doys, read_spec) %>%
      reduce(full_join, by = "wavelength")

    waves <- pull(spec_dat, wavelength)
    spectra <- spec_dat %>%
      select(-wavelength) %>%
      as.matrix() %>%
      spectra(waves, "R")

    if (is_HF) {
        trans_flist <- list.files(transpath, full.names = TRUE)
        stopifnot(length(trans_flist) == length(doys))

        trans_dat <- map2(trans_flist, doys, read_spec) %>%
          reduce(full_join, by = "wavelength")

        twaves <- pull(trans_dat, wavelength)
        tspectra <- trans_dat %>%
          select(-wavelength) %>%
          as.matrix() %>%
          spectra(twaves, "T")
        spectra <- cbind(spectra, tspectra)
    }

    list(metadata = metadata, spectra = spectra)
}

##options(error = recover)
result <- map2(c(2011, 2012), c("MV_MA", "HF_MA"), readYang)

all_metadata <- map_dfr(result, "metadata") %>%
  mutate(
    instrument_code = "unknown",
    is_experiment = FALSE
  ) %>%
  mutate_at(
    vars(ends_with("pct_mass")),
    add_metadata,
    data_unit = "%"
  ) %>%
  mutate_at(
    vars(ends_with("per_area"), -starts_with("leaf_mass")),
    add_metadata,
    data_unit = "ug cm-2"
  ) %>%
  add_column_metadata(
    leaf_mass_per_area = list(data_unit = "g m-2")
  ) %>%
  add_metadata(!!!project_metadata)

spectra <- map(result, "spectra") %>% do.call(cbind, .)
spectra[spectra > 1 | spectra <= 0] <- NA

create_project(
  specdb = specdb,
  project_code = project_code,
  spectra = spectra,
  metadata = all_metadata,
  overwrite = TRUE
)
