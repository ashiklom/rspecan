library(rspecan)
library(ggplot2)
library(tidyverse)
import::from("ggsci", "pal_ucscgb")
specdb <- indir("spectra_db")

dat <- get_metadata(specdb)
proj_colors <- read_csv(file.path(specdb, "project_colors.csv"))

dat_geog <- dat %>%
  distinct(project_code, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))

ggplot(dat_geog) +
  aes(x = longitude, y = latitude, color = project_code) +
  borders(fill = "white", colour = "black") +
  geom_point() +
  scale_color_manual(values = df2dict(proj_colors))

ggsave(infile("manuscript", "figures", "data_map.pdf"))
