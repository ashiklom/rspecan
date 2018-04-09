library(rspecan)
library(tidyverse)
library(riri)
import::from("lubridate", "parse_date_time", "days")

specdb <- indir("spectra_db")

all_meta <- get_metadata(specdb)

sites <- all_meta %>%
  distinct(latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(site_name = sprintf("site_%03d", row_number()))

precip_src <- "SOURCES .NOAA .NCEP .CPC .Merged_Analysis .monthly .latest .ver2 .prcp_est"
precip_raw <- get_tabular(precip_src, sites)

precip_tidy <- precip_raw %>%
  mutate(
    date = parse_date_time(`T`, "my") + days(15)
  ) %>%
  select(-`T`) %>%
  gather("site_name", "precipitation", -date)

sites %>%
  left_join(precip_tidy) %>%
  select(latitude, longitude, date, precipitation) %>%
  write_csv(infile("extdata", "latlon_precip.csv"))
