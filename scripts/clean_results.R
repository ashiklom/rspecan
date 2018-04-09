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
    any_vars(. > quantile(., 0.95, na.rm = TRUE))
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

# Fill in more trait measurements
fill_trait <- function(x, y, mult) {
  case_when(
    !is.na(x) ~ x,
    !is.na(y) & !is.na(mult) ~ y * mult,
    TRUE ~ NA_real_
  )
}
md <- md %>%
  mutate(
    leaf_anth_per_area = if_else(leaf_anth_per_area < 0, NA_real_, leaf_anth_per_area),
    leaf_N_per_area = fill_trait(leaf_N_per_area, leaf_N_pct_mass, leaf_mass_per_area * 1e5),
    leaf_C_per_area = fill_trait(leaf_C_per_area, leaf_C_pct_mass, leaf_mass_per_area * 1e5),
    leaf_chla_per_area = fill_trait(leaf_chla_per_area, leaf_chla_pct_mass, leaf_mass_per_area * 1e6),
    leaf_chlb_per_area = fill_trait(leaf_chlb_per_area, leaf_chlb_pct_mass, leaf_mass_per_area * 1e6),
    leaf_chltot_per_area = if_else(is.na(leaf_chltot_per_area), leaf_chla_per_area + leaf_chlb_per_area, leaf_chltot_per_area),
    leaf_nonpolar_per_area = leaf_nonpolar_pct_mass * leaf_mass_per_area * 1e5,
    leaf_polar_per_area = leaf_polar_pct_mass * leaf_mass_per_area * 1e5,
    leaf_cellulose_per_area = leaf_cellulose_pct_mass * leaf_mass_per_area * 1e5,
    leaf_lignin_per_area = leaf_lignin_pct_mass * leaf_mass_per_area * 1e5,
    leaf_H_per_area = leaf_H_pct_mass * leaf_mass_per_area * 1e5,
    leaf_lutein_per_area = leaf_lutein_pct_mass * leaf_mass_per_area * 1e5,
    leaf_neoxanthin_per_area = leaf_neoxanthin_pct_mass * leaf_mass_per_area * 1e5,
    leaf_betacarotene_per_area = leaf_betacarotene_pct_mass * leaf_mass_per_area * 1e5,
    leaf_fiber_per_area = leaf_fiber_pct_mass * leaf_mass_per_area * 1e5,
    leaf_ash_per_area = leaf_ash_pct_mass * leaf_mass_per_area * 1e5,
    leaf_fat_per_area = leaf_fat_pct_mass * leaf_mass_per_area * 1e5,
    leaf_NSC_per_area = leaf_NSC_pct_mass * leaf_mass_per_area * 1e5,
    leaf_protein_per_area = leaf_protein_pct_mass * leaf_mass_per_area * 1e5,
    leaf_K_per_area = leaf_K_pct_mass * leaf_mass_per_area * 1e5,
    leaf_O_per_area = leaf_O_pct_mass * leaf_mass_per_area * 1e5,
    leaf_starch_per_area = leaf_starch_pct_mass * leaf_mass_per_area * 1e5
  ) %>%
  select_if(~!all(is.na(.)))

#md %>%
  #select(matches("leaf_.*_per_area")) %>%
  #pairs() ## Or...
  #gather(trait, value) %>%
  #ggplot() +
  #aes(x = 1, y = value) +
  #geom_boxplot() +
  #facet_wrap(~trait, scales = "free")

write_csv(results, "spectra_db/cleaned_results.csv")
write_csv(bad_results, "spectra_db/bad_results.csv")
write_csvy(md, "spectra_db/cleaned_metadata.csvy")
write_csvy(bad_md, "spectra_db/bad_metadata.csvy")
