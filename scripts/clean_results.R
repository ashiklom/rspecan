library(tidyverse)

raw_results <- read_csv("spectra_db/all_results.csv") %>%
  select(-specdb)

drop_results <- raw_results %>%
  select(project_code, observation_id, prospect_version, parameter, Mean) %>%
  spread(parameter, Mean) %>%
  filter(
    N > 12 | Cab > 200 | Car > 100 | Cw > 0.2 | Cm > 0.2
  ) %>%
  distinct(projet_code, observation_id, prospect_version)

results <- anti_join(raw_results, drop_results)
bad_results <- semi_join(raw_results, drop_results)

write_csv(results, "spectra_db/cleaned_results.csv")
write_csv(bad_results, "spectra_db/bad_results.csv")
