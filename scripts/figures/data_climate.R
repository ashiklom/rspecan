library(rspecan)
library(tidyverse)
import::from(metar, read_csvy)

data(biome_labels, biome_polygons, package = "rspecan")

clim_data <- readRDS(infile("extdata", "worldclim_met.rds")) %>%
  unnest(met_data) %>%
  select(-latitude1, -longitude1, -site_name) %>%
  filter(AMT > -1e5) %>%
  mutate(
    AP = AP / 10
  )

sites <- read_csvy(infile("spectra_db", "cleaned_metadata.csvy")) %>%
  distinct(project_code, short_name, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  left_join(clim_data %>% select(latitude, longitude, AMT, AP))

project_colors <- read_csv(infile("spectra_db", "project_colors.csv")) %>%
  semi_join(sites)

sites <- sites %>% semi_join(project_colors)

plt <- ggplot() +
  geom_polygon(
    data = biome_polygons,
    mapping = aes(x = temp_degC, y = prec_mm, fill = biome)
  ) +
  geom_label(
    data = biome_labels,
    mapping = aes(x = y, y = x, label = biome_label)
  ) +
  geom_point(
    data = sites,
    mapping = aes(x = AMT, y = AP, color = short_name),
    size = 3
  ) +
  scale_fill_manual(values = df2dict(biome_labels, "color", "biome") %>% as.character()) +
  scale_color_manual(values = df2dict(project_colors, "color", "short_name")) +
  xlab(expression("Annual mean temperature" ~ (degree * C))) +
  ylab("Annual precipitation (cm)") +
  guides(fill = FALSE) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
ggsave(
  infile("manuscript", "figures", "data_climate.pdf"),
  plt,
  width = 10,
  height = 7,
  units = "in"
)
