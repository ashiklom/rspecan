library(tidyverse)
library(rspecan)
library(corrplot)
import::from(metar, read_csvy)

results <- read_csv("spectra_db/cleaned_results.csv")

results_wide <- results %>%
  filter(
    prospect_version == "D",
    parameter %in% variable_df$code
  ) %>%
  mutate(parameter = factor(parameter, variable_df$code)) %>%
  select(project_code, observation_id, parameter, Mean) %>%
  spread(parameter, Mean)

metadata <- read_csvy("spectra_db/cleaned_metadata.csvy") %>%
  mutate_if(is.character, na_if, "")

dat <- left_join(results_wide, metadata) %>%
  select(
    project_code, observation_id, species_code,
    !!!variable_df$code, starts_with("leaf"),
    -leaf_temperature, -leaf_age,
    -leaf_anth_per_area, -leaf_prospect_N, -leaf_area, -leaf_thickness
  )

order_vals <- . %>%
  select(
    !!!df2dict(variable_df, "code", "shortname"),
    ends_with("per_area"),
    ends_with("_area"),
    leaf_water_thickness,
    ends_with("pct_mass"),
    ends_with("_mass"),
    everything()
  )


dat_vals <- dat %>%
  select(-project_code, -observation_id, -species_code) %>%
  order_vals
all_corr <- cor(dat_vals, use = "pairwise.complete.obs")[, variable_df$shortname]

pdf(infile("manuscript/figures/trait_correlations_all.pdf"))
corrplot(all_corr)
dev.off()

species_means <- dat %>%
  select(-project_code, -observation_id) %>%
  group_by(species_code) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  select_if(~sum(!is.na(.)) > 3)

species_vals <- species_means %>%
  select(-species_code) %>%
  order_vals
species_corr <- cor(species_vals, use = "pairwise.complete.obs")[, variable_df$shortname]
pdf(infile("manuscript/figures/trait_correlations_species.pdf"))
corrplot(species_corr)
dev.off()
