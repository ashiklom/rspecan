library(tidyverse)
library(rspecan)
import::from("knitr", "kable")
import::from("metar", "read_csvy")

dat <- read_csvy("spectra_db/cleaned_metadata.csvy")

dat_proj <- dat %>%
  group_by(short_name, long_name) %>%
  summarize(
    Samples = n(),
    Species = n_distinct(species_code),
    Sites = n_distinct(latitude, longitude)
  ) %>%
  rename("Short name" = short_name, "Long name" = long_name)

write_csv(dat_proj, infile("manuscript/figures/project_table.csv"))

kable(dat_proj, format = "latex") %>%
  cat(file = infile("manuscript", "figures", "project_table.tex"))
