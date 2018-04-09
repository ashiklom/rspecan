library(rspecan)
library(tidyverse)
library(metar)
library(riri)

specdb <- indir("spectra_db")

all_meta <- get_metadata(specdb)

sites <- all_meta %>%
  distinct(latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(site_name = sprintf("site_%03d", row_number()))
  #unite(site_name, project_code, site_code, plot_code, sep = "|", remove = FALSE)

sites_segmented <- sites %>%
  mutate(group = row_number() %/% 20) %>%
  select(group, site_name, latitude, longitude) %>%
  group_by(group) %>%
  nest()

temp_source <- ncep_var("air_temperature")
temp_segmented <- sites_segmented %>%
  mutate(met_data = map(data, get_tabular, source_string = temp_source))
temp_data <- reduce(temp_segmented$met_data, full_join, by = "T")

temp_tidy <- temp_data %>%
  mutate(
    date = lubridate::parse_date_time(`T`, "my") + lubridate::days(15)
  ) %>%
  select(-`T`) %>%
  gather(key = "site_name", value = "air_temperature_K", -date)

temp_tidy %>%
  right_join(sites) %>%
  select(latitude, longitude, date, air_temperature_K) %>%
  write_csv(infile("extdata", "latlon_air_temperature.csv"))
