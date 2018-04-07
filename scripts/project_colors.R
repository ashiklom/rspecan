library(rspecan)
library(tidyverse)
import::from("ggsci", "pal_ucscgb")
import::from(metar, read_csvy)

specdb <- indir("spectra_db")
dat <- read_csvy(infile(specdb, "cleaned_metadata.csvy"))

proj_colors <- dat %>%
  distinct(project_code, short_name) %>%
  mutate(color = pal_ucscgb()(nrow(.))) %>%
  add_row(project_code = "all", short_name = "Overall", color = "black", .before = 1)
write_csv(proj_colors, infile(specdb, "project_colors.csv"))
