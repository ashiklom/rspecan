library(tidyverse)
library(rspecan)
library(metar)

results <- read_csv("spectra_db/all_results.csv")
metadata <- get_metadata("spectra_db")
species_info <- read_csvy("spectra_db/species_info.csvy")

md_spp <- metadata %>%
  select(-genus, -species, -scientific_name, -variety) %>%
  left_join(species_info) %>%
  select(project_code, observation_id, !!!colnames(species_info))

dat <- left_join(results, md_spp)

species_summary <- dat %>%
  filter(species_code != "") %>%
  group_by(species_code, prospect_version, parameter) %>%
  summarize(
    Mean = mean(Mean, na.rm = TRUE),
    SD = sd(Mean, na.rm = TRUE),
    Min = min(Mean, na.rm = TRUE),
    Max = max(Mean, na.rm = TRUE)
  )
