library(daymetr)
library(rspecan)
library(tidyverse)
library(metar)
library(riri)

all_meta <- get_metadata("spectra_db")

sites <- all_meta %>%
  distinct(site_code, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))

dir.create("extdata/daymet", recursive = TRUE, showWarnings = FALSE)
#daymet_data <- download_daymet_batch(tmp, 1990, 2010, path = "extdata/daymet")

#daymet_summary <- map_dfr(daymet_data, summarize_daymet)

#ggplot(daymet_summary) +
  #aes(x = longitude, y = latitude, color = annual_precip_total) +
  #borders() +
  #geom_point()
