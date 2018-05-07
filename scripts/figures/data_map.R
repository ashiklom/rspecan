library(rspecan)
library(ggplot2)  # Might need the GitHub version for geom_sf, coord_sf
library(tidyverse)
library(rnaturalearth)
library(sf)
import::from("metar", "read_csvy")
import::from(broom, tidy)
specdb <- indir("spectra_db")

dat <- read_csvy(infile(specdb, "cleaned_metadata.csvy"))
proj_colors <- read_csv(file.path(specdb, "project_colors.csv")) %>%
  df2dict("color", "short_name")

robinson <- st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
gall_stereo <- st_crs("+proj=gall +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

my_proj <- gall_stereo
wgs <- st_crs(4326)

dat_geog <- dat %>%
  distinct(project_code, short_name, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(geometry = map2(longitude, latitude, ~st_point(c(.x, .y)))) %>%
  st_sf(crs = wgs) %>%
  st_transform(my_proj) %>%
  group_by(project_code) %>%
  mutate(rn = row_number(), N = n()) %>%
  ungroup() %>%
  arrange(desc(rn), desc(N))

mapdat_sf <- ne_countries(returnclass = "sf") %>%
  st_transform(my_proj) %>%
  as_tibble() %>%
  st_as_sf()

y_lim <- c(-15, 70)
x_lim <- c(-160, 50)
xy_lim <- cbind(x_lim, y_lim) %>%
  st_multipoint() %>%
  st_sfc(crs = wgs) %>%
  st_transform(my_proj) %>%
  st_bbox()
plt <- ggplot(dat_geog) +
  geom_sf(data = mapdat_sf, inherit.aes = FALSE, fill = "grey50", color = NA) +
  geom_sf(aes(fill = short_name), color = "black", size = 3, pch = 21) +
  coord_sf(xlim = xy_lim[c("xmin", "xmax")], ylim = xy_lim[c("ymin", "ymax")]) +
  scale_fill_manual(values = proj_colors) +
  guides(fill = guide_legend(title = element_blank(), override.aes = list(color = NA))) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.line = element_blank(),
    panel.grid = element_line(color = NA)
  )
ggsave(infile("manuscript", "figures", "data_map.pdf"), plt, width= 7, height = 5)
