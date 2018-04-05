library(rspecan)
library(ggplot2)
library(tidyverse)
import::from("ggsci", "pal_ucscgb")
specdb <- indir("spectra_db")

dat <- get_metadata(specdb)
proj_colors <- read_csv(file.path(specdb, "project_colors.csv")) %>%
  df2dict("color", "short_name")

dat_geog <- dat %>%
  distinct(project_code, short_name, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))

plt <- ggplot(dat_geog) +
  aes(x = longitude, y = latitude, color = short_name) +
  borders(fill = "white", colour = "black") +
  geom_point() +
  scale_color_manual(values = proj_colors) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(infile("manuscript", "figures", "data_map.pdf"), plt)
