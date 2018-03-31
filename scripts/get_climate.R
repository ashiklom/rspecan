library(rspecan)
library(riri)
library(tidyverse)

specdb <- indir("spectra_db")

all_meta <- get_metadata(specdb)

sites <- all_meta %>%
  distinct(latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))

gws <- function(lat, lon, pb) {
  pb$tick()
  get_worldclim_site(lat, lon)
}

pb <- progress::progress_bar$new(total = nrow(sites), format = "[:bar] :current / :total ")
site_met <- sites %>%
  mutate(met_data = map2(latitude, longitude, possibly(gws, NULL), pb = pb))
saveRDS(site_met, infile("extdata", "worldclim_met.rds"))
