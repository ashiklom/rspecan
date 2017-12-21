source(here::here("manuscript", "setup.R"))

## ---- datamap ----------
data(biome_labels)
data(biome_polygons)
mapdat <- results_raw %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct(latitude, longitude, project)

leg_guide <- guides(color = guide_legend(nrow = 7, title = NULL))
leg_theme <- theme(
  legend.position = "bottom"
)

worldmap_raw <- maps::map("world", fill = TRUE, plot = FALSE)
worldmap <- as_tibble(broom::tidy(worldmap_raw))

dat_map <- ggplot(mapdat) +
  aes(x = longitude, y = latitude, color = project) +
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "white", color = "grey50", data = worldmap) +
  geom_point() +
  coord_cartesian(xlim = c(-180, 45), ylim = c(-20, 70)) +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

project_legend <- get_legend(dat_map + leg_guide + leg_theme)
plot_grid(dat_map, project_legend, nrow = 2, rel_heights = c(1, 0.4))
