library(rspecan)
library(riri)
library(tidyverse)
import::from(metar, read_csvy)

old_site_met <- tryCatch(
  readRDS(infile("extdata", "worldclim_met.rds")),
  error = function(e) tibble(latitude = NA_real_, longitude = NA_real_)
)

specdb <- indir("spectra_db")

all_meta <- read_csvy(infile(specdb, "cleaned_metadata.csvy"))

all_sites <- all_meta %>%
  distinct(latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))

new_sites <- anti_join(all_sites, old_site_met)

gws <- function(lat, lon, pb) {
  pb$tick()
  get_worldclim_site(lat, lon)
}

pb <- progress::progress_bar$new(total = nrow(new_sites), format = "[:bar] :current / :total ")
new_site_met <- new_sites %>%
  mutate(met_data = map2(latitude, longitude, possibly(gws, NULL), pb = pb))

site_met <- bind_rows(old_site_met, new_site_met)
saveRDS(site_met, infile("extdata", "worldclim_met.rds"))
