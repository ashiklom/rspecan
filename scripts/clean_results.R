#!/usr/bin/env Rscript
library(tidyverse)
library(rspecan)
import::from("metar", "write_csvy")

barnes_results <- read_csv("spectra_db/barnes_results.csv")

raw_results <- read_csv("spectra_db/all_results.csv") %>%
  select(-specdb) %>%
  filter(project_code != "barnes_2017") %>%
  bind_rows(barnes_results)

all_md <- get_metadata(
  "spectra_db",
  metadata_cols = c("project_code", "short_name", "long_name")
)

drop_md <- all_md %>%
  filter(
    target_type %in% c("rock", "soil") | fresh_dry == "dry"
  )

drop_results <- raw_results %>%
  select(project_code, observation_id, prospect_version, parameter, Mean) %>%
  spread(parameter, Mean) %>%
  filter_at(
    c("N", "Cab", "Car", "Canth", "Cbrown", "Cw", "Cm"),
    any_vars(. > quantile(., 0.975, na.rm = TRUE))
  ) %>%
  distinct(project_code, observation_id, prospect_version)

results <- raw_results %>%
  anti_join(drop_results) %>%
  anti_join(drop_md)
bad_results <- anti_join(raw_results, results)

if (interactive()) {
  ggplot(results) +
    aes(x = prospect_version, y = Mean) +
    geom_boxplot() +
    facet_wrap(~parameter, scales = "free")
}

md <- semi_join(all_md, results)
bad_md <- anti_join(all_md, results)

write_csv(results, "spectra_db/cleaned_results.csv")
write_csv(bad_results, "spectra_db/bad_results.csv")
write_csvy(md, "spectra_db/cleaned_metadata.csvy")
write_csvy(bad_md, "spectra_db/bad_metadata.csvy")
