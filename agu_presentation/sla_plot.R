## ---- sla_setup ----------
library(tidyverse)

spp_file <- "~/Projects/new-phytologist-traits/preprocess-try/pfts_species/tps_species.rds"
data_file <- "~/Projects/new-phytologist-traits/np-trait-analysis/extdata/trait_data.rds"

try_spp <- readRDS(spp_file)
try_data <- readRDS(data_file)
early_hardwood <- tribble(
  ~AccSpeciesName,
  "Acer barbatum",
  "Acer grandidentatum",
  "Acer leucoderme",
  "Acer negundo",
  "Acer nigrum",
  "Acer pensylvanicum",
  "Acer platanoides",
  "Acer rubrum",
  "Acer saccharinum",
  "Acer saccharum",
  "Acer spicatum",
  "Aesculus flava",
  "Alnus glutinosa",
  "Alnus rhombifolia",
  "Calycanthus floridus",
  "Carpinus caroliniana",
  "Cornus florida",
  "Diospyros texana",
  "Diospyros virginiana",
  "Fagus grandifolia",
  "Halesia carolina",
  "Halesia parviflora",
  "Ilex montana",
  "Oxydendrum arboreum",
  "Platanus occidentalis",
  "Symplocos tinctoria",
  "Tilia americana",
  "Fagus sylvatica"
) %>%
  mutate(
    label = gsub("^([[:alpha:]])([[:alpha:]]+)", "\\1.", AccSpeciesName)
  )
sp_ids <- try_spp %>%
  inner_join(early_hardwood) %>%
  select(species = label, AccSpeciesID)

sp_compare <- sp_ids %>%
  inner_join(try_data) %>%
  mutate(
    species = factor(species) %>%
      forcats::fct_reorder(LMA, median, na.rm = TRUE)
  ) %>%
  arrange(species)

sp_means <- sp_compare %>%
  group_by(species) %>%
  summarize(LMA = median(LMA, na.rm = TRUE))

x_lim <- c(1, n_distinct(sp_compare$species))
xseq <- seq(x_lim[1], x_lim[2])
y_lim <- c(0, 0.2)
ylab <- expression("Leaf mass per area" ~ (kg ~ m^{-2}))

SLA_bdt <- 30

SLA_other <- c(
  "NET temp" = 10,
  "NET bo" = 8,
  "NDT bo" = 24,
  "BET trop" = 12
)

opts_chunk$set(fig.dim = c(4.35, 3.30))
#opts_chunk$set(dependson = "sla_setup")


## ---- sla_base ----------
par(cex = 0.7, mar = c(4.5, 4.5, 0.1, 0.1))
plot(0, 0, type = "n", xlim = x_lim, ylim = y_lim,
      xlab = NA, ylab = ylab, xaxt = "n")
axis(1, at = xseq, labels = FALSE, las = 2, tck = -0.01)
text(xseq, par("usr")[3], labels = sp_means$species, xpd = TRUE,
      srt = 45, adj = c(1.1, 1.6), cex = 0.7)

## ---- sla_pft1 ----------
abline(h = 1 / SLA_bdt, lty = "dashed", col = "red")

## ---- sla_pft2 ----------
abline(h = 1 / SLA_other, lty = "dashed", col = "blue")

## ---- sla_points ----------
points(xseq, sp_means$LMA, pch = 19, col = "black")

## ---- sla_box ----------
boxplot(LMA ~ species, data = sp_compare, outline = FALSE, 
        xlab = "", ylab = ylab, xaxt = "n", add = TRUE,
        medlwd = 0, col = "white")
points(xseq, sp_means$LMA, pch = 19, col = "black")

## ---- map_prep ----------
get_coords <- function(param) {
  try_data %>%
    filter_at(c(param, "Latitude", "Longitude"), all_vars(!is.na(.))) %>%
    distinct(Latitude, Longitude)
}
traitmap <- function(param, col, ...) {
  points(Latitude ~ Longitude, data = get_coords(param), col = col, ...)
}
opts_chunk$set(fig.dim = c(2.5, 0.9))

## ---- map_base ----------
par(pch = 19, cex = 0.1)
maps::map("world", col = "grey50", resolution = 0, lwd = 0.5, mar = rep(0, 4))

## ---- sla_map ----------
traitmap("SLA", "red")

## ---- leafn_map ----------
traitmap("Narea", "purple")

## ---- vcmax_map ----------
traitmap("Vcmax_area", "blue")
