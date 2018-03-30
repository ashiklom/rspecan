library(rspecan)
library(tidyverse)
import::from("ggsci", "pal_ucscgb")

specdb <- indir("spectra_db")
dat <- get_metadata(specdb)

proj_colors <- dat %>%
  distinct(project_code) %>%
  mutate(color = pal_ucscgb()(nrow(.)))
write_csv(proj_colors, infile(specdb, "project_colors.csv"))
