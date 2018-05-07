library(tidyverse)
library(rspecan)
library(metar)

species_raw <- read_csv(
  "spectra_db/species_info_orig.csvy",
  comment = "#",
  col_types = cols(.default = "c", nitrogen_fixer = "l", shade_tolerance_numeric = "c",
                   try_species_ID = "i", myco_is_am = "l", is_shade_intolerant = "l")
)

unknowns <- tribble(
  ~species_code, ~growth_form,
  "ACMI2", "herb",
  "AMCA6", "shrub",
  "DACA7", "herb",
  "SONU2", "graminoid",
  "KOMA", "graminoid",
  "LUPE3", "herb",
  "OLRIR", "herb",
  "LECA8", "herb",
  "LIAS", "herb",
  "DAVI", "herb",
  "POPR", "graminoid",
  "SCSC", "graminoid",
  "PAVI2", "graminoid",
  "ASTU", "herb",
  "MOFI", "herb",
  "DAPU5", "herb",
  "SANEN", "herb",
  "TACHIGALI", "tree",
  "OCOTE", "tree",
  "ARGL", "herb",
  "CESP", "tree",
  "SALE", "shrub",
  "CADE", "herb",
  "BAPI", "shrub",
  "HEAR", "shrub",
  "CEME", "shrub",
  "ASSY", "herb"
)

species_dup <- species_raw %>%
  bind_rows(unknowns) %>%
  group_by(species_code) %>%
  filter(n() > 1) %>%
  arrange(species_code)

fix_bygroup <- function(x) {
  if (length(x) == 1) return(x)
  if (all(is.na(x))) return(x[1])
  xn <- x[!is.na(x)]
  if (length(xn) == 1) return(xn)
  ux <- unique(xn)
  if (length(ux) == 1) return(ux)
  if (is.logical(ux) && any(!ux)) return(FALSE)
  warning("Multiple values detected. Returning first one.")
  return(ux[1])
}

species_dfixed <- species_dup %>%
  summarize_all(fix_bygroup)

species_info <- species_raw %>%
  anti_join(species_dfixed, by = "species_code") %>%
  bind_rows(species_dfixed)

species_info %>%
  count(species_code, sort = TRUE) %>%
  filter(n > 1) %>%
  nrow() %>%
  `==`(0) %>%
  stopifnot()

write_csv(species_info, "spectra_db/species_info.csvy")
