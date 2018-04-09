library(tidyverse)
library(ggforce)
dat <- readRDS("processed_results/complete_results.rds")

geog_dat <- dat %>%
  filter(!is.na(N_mid), !is.na(latitude), !is.na(longitude)) %>%
  count(project, latitude, longitude) %>%
  mutate(project = factor(project))

#layout(matrix(1:2, ncol = 1), heights = c(1, 0.6))
maps::map("world", resolution = 0, col = "grey40", mar = c(1, 1, 1, 1))
points(x = geog_dat$longitude,
       y = geog_dat$latitude,
       col = as.integer(geog_dat$project),
       pch = 19, cex = 0.7)
