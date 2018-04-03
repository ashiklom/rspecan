#!/usr/bin/env Rscript
library(tidyverse)
library(rspecan)
library(metar)

species_info <- readRDS(infile("extdata", "usda_try.rds")) %>%
  bind_rows(read_csv(infile("extdata", "species_other.csv"))) %>%
  as_tibble() %>%
  mutate(
    Database_ID = str_replace(codetype, "_", " ")
  ) %>%
  select(
    species_code = code,
    scientific_name = scientificname,
    family, genus, species, subspecies,
    variety, subvariety, forma,
    Database_ID
  ) %>%
  full_join(read_csv(infile("extdata", "species_attributes.csv")))

species_info2 <- species_info %>%
  mutate_if(
    is.character,
    ~na_if(., "")
  ) %>%
  mutate(
    growth_form = factor(
      growth_form,
      c("tree", "shrub", "herb", "graminoid", "vine", "lichen")
    ),
    leaf_type = factor(
      leaf_type,
      c("broad", "needle")
    ),
    myco_is_am = myco_asso %in% c("AM + NM", "AM", "NM", "AM?"),
    phenology = factor(phenology, c("deciduous", "evergreen")),
    ps_type = factor(ps_type, c("C3", "C4")),
    shade_tolerance = tolower(shade_tolerance) %>%
      factor(c("intolerant", "intermediate", "tolerant")),
    is_shade_intolerant = shade_tolerance == "intolerant"
  )

write_csvy(species_info2, infile("spectra_db/species_info.csvy"))
