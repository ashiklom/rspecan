library(rspecan)
library(tidyverse)
import::from("ggsci", "pal_ucscgb")

specdb <- indir("spectra_db")
dat <- read_csvy(infile(specdb, "cleaned_metadata.csv"))

proj_colors <- dat %>%
  distinct(project_code, short_name) %>%
  mutate(color = pal_ucscgb()(nrow(.)))
write_csv(proj_colors, infile(specdb, "project_colors.csv"))
