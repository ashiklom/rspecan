## ---- methods_setup ----------
library(tidyverse)
library(PEcAnRTM)
library(rspecan)
data_file <- "../processed_results/complete_results.rds"
source("paultolcolors.R")
project_info <- tribble(
  ~long_project, ~project,
  "ACCP", "accp",
  "ANGERS", "angers",
  "LOPEX", "lopex",
  "Di Vittorio et al. 2009", "divittorio_conifer",
  "California traits", "ecosis_californiatraits",
  "Cedar Creek biodiv.", "ecosis_cedarcreek_biodiversity",
  "Corn varieties", "ecosis_cornvarieties",
  "Hawaii 2000", "ecosis_hawaii2000",
  "Milkweed stress", "ecosis_milkweed_stress",
  "Missola time series", "ecosis_missoulats",
  "Pepper", "ecosis_pepper",
  "PVY potato", "ecosis_pvy_solanum",
  "Santa Monica", "ecosis_santamonica",
  "Soybean aphid", "ecosis_soybean_aphid",
  "Spec. var. leaf/canopy", "ecosis_spectral_variation_leafcanopy",
  "Foster et al. 2017", "foster_beetle",
  "NASA FFT", "nasa_fft",
  "NASA HyspIRI", "nasa_hyspiri",
  "NGEE", "ngee_arctic",
  "NGEE", "ngee_tropics",
  "Wu et al. 2016", "wu_brazil",
  "Yang et al. 2016", "yang_pheno"
)
project_colors <- project_info %>%
  distinct(long_project) %>%
  mutate(project_color = tol21rainbow)

results <- readRDS(data_file) %>%
  left_join(project_info) %>%
  left_join(project_colors)
blankplot <- function(...) {
  plot(0, 0, type = "n", ann = FALSE, axes = FALSE, frame.plot = FALSE, ...)
}
not_missing <- function(dat) select_if(dat, ~!all(is.na(.)))

## ---- data_map ----------
map_dat <- results %>%
  distinct(long_project, latitude, longitude, project_color)
par(cex = 0.7)
maps::map("world", resolution = 0, mar = c(0, 0, 0, 0), col = "grey30")
points(latitude ~ longitude, data = map_dat, col = map_dat$project_color,
       pch = 20)

## ---- project_legend ----------
par(mar = rep(0, 4), cex = 0.6)
blankplot()
legend(
  "center",
  legend = project_colors$long_project,
  col = project_colors$project_color,
  pch = 19,
  ncol = 3
)

## ---- milkweed_setup ----------
milkweed <- results %>%
  filter(!is.na(treatment_water), !is.na(treatment_temperature)) %>%
  mutate(
    treatment_water = factor(treatment_water, c("Well-watered", "Water stressed")),
    treatment_temperature = factor(treatment_temperature, c(23, 30))
  ) %>%
  unite(treatment, treatment_temperature, treatment_water, remove = FALSE) %>%
  select_if(~!all(is.na(.))) %>%
  select(-ends_with("_hi"), -ends_with("_lo"))

milkweed_file <- "../processed_data/ecosis_milkweed_stress.rds"
milkweed_spec <- readRDS(milkweed_file)$spec
ww_ids <- milkweed %>%
  filter(treatment_water == "Well-watered") %>%
  pull(spectra_id)
ws_ids <- milkweed %>%
  filter(treatment_water == "Water stressed") %>%
  pull(spectra_id)
ww_spec <- milkweed_spec[, ww_ids]
ws_spec <- milkweed_spec[, ws_ids]
spec_smry <- function(x) t(apply(x, 1, quantile, c(0.1, 0.5, 0.9), na.rm = TRUE))
ww_spec_smry <- spec_smry(ww_spec)
ws_spec_smry <- spec_smry(ws_spec)

treatment_colors <- c("green3", "orange")

bp <- function(p, main = p, ...) {
  form <- sprintf("as.numeric(`%s`) ~ treatment_water", p)
  boxplot(as.formula(form), data = milkweed, outline = FALSE,
          xaxt = "n", ylab = "", main = main,
          col = treatment_colors, ...)
}

spec_params <- c("N_mid", "Cab_mid", "Car_mid", "Canth_mid",
                 "Cw_mid", "Cm_mid")
spec_params_proper <- list(
  expression("# mesophyll"),
  expression("Chlorophyll" ~ (mu*g ~ cm^{-2})),
  expression("Carotenoids" ~ (mu*g ~ cm^{-2})),
  expression("Anthocyanins" ~ (mu*g ~ cm^{-2})),
  expression("Water" ~ (g ~ cm^{-2})),
  expression("Dry matter" ~ (g ~ cm^{-2}))
)

field_traits <- milkweed %>%
  select_if(is.double) %>%
  select(-one_of(spec_params), -Cbrown_mid, -residual_mid, -ends_with("pct_mass")) %>%
  colnames()
field_traits_proper <- list(
  expression("Leaf mass per area" ~ (g ~ m^{-2})),
  expression("C:N ratio"),
  expression("C"["area"] ~ (g ~ m^{-2})),
  expression("N"["area"] ~ (g ~ m^{-2})),
  expression(V[list(c,max)] ~ (mu * mol ~ m^{-2} ~ s^{-1})),
  expression("Fiber" ~ (g ~ m^{-2})),
  expression("Lignin" ~ (g ~ m^{-2}))
)
par_box <- list(mfrow = c(4, 2), mar = c(1, 2, 2, 1.5), mgp = c(2.2, 0.5, 0),
                cex = 0.4, cex.main = 1.1, cex.axis = 0.8)

## ---- field_trait_plot ----------
par(par_box)
.z <- Map(bp, field_traits, field_traits_proper)
blankplot()
legend(
  "center",
  legend = c("Well-watered", "Water stressed"),
  col = treatment_colors,
  pch = 19,
  bty = "n"
)

## ---- spec_trait_plot ----------
par(par_box)
.z <- Map(bp, spec_params, spec_params_proper)

## ---- milkweed_spec_plot ----------
milkweed_spec_plot <- function(x, ...) {
  lty <- c("dashed", "solid", "dashed")
  matplot(wl_milkweed, x, type = "l", add = TRUE, lty = lty, ...)
}
ylim <- range(c(ww_spec_smry, ws_spec_smry))
wl_milkweed <- 350:2500
wl_range <- c(450, 700)
par(cex = 0.6, cex.axis = 0.6, cex.lab = 0.6, mar = c(2, 2, 0.1, 0.1),
    mgp = c(0.8, 0.2, 0), tcl = -0.1)
plot(0, 0, type = "n", xlab = "", ylab = "",
     xlim = wl_range, ylim = c(0, 0.25))
milkweed_spec_plot(ww_spec_smry, col = treatment_colors[1], lwd = 0.9)
milkweed_spec_plot(ws_spec_smry, col = treatment_colors[2], lwd = 0.9)

## ---- fertilization_setup ----------
fert <- results %>%
  filter(!is.na(N_fertilization_treatment)) %>%
  select_if(~!all(is.na(.))) %>%
  mutate(
    N_fertilization_treatment = recode_factor(
      N_fertilization_treatment,
      `Not Fertalized` = "Not fertilized",
      `Fertalized` = "Fertilized"
    )
  )

fert_colors <- c("orange", "purple")
bp <- function(p, main = p, ...) {
  form <- sprintf("as.numeric(`%s`) ~ N_fertilization_treatment", p)
  boxplot(as.formula(form), data = fert, outline = FALSE,
          xaxt = "n", ylab = "", main = main,
          col = fert_colors, ...)
}

## ---- fertilization_spec ----------
fert_file <- "../processed_data/ecosis_spectral_variation_leafcanopy.rds"
fert_spec <- readRDS(fert_file)$spectra
ff <- fert %>%
  filter(N_fertilization_treatment == "Fertilized") %>%
  pull(spectra_id)
nf <- fert %>%
  filter(N_fertilization_treatment == "Not fertilized") %>%
  pull(spectra_id)
ff_spec <- fert_spec[, ff]
nf_spec <- fert_spec[, nf]
wl_fert <- 350:2500

par(cex = 0.6, cex.lab = 0.9, cex.axis = 0.7, mar = c(2, 2, 0.1, 0.1),
    tcl = -0.3, mgp = c(1, 0.3, 0))
ylim <- range(c(ff_spec, nf_spec))
plot(0, 0, type = "n", xlab = "Wavelength (nm)", ylab = "Reflectance",
     xlim = range(wl_fert), ylim = ylim)
matplot(wl_fert, nf_spec, type = "l", lty = "solid", col = fert_colors[1], add = TRUE)
matplot(wl_fert, ff_spec, type = "l", lty = "solid", col = fert_colors[2], add = TRUE)

## ---- fertilization_plot ----------
par_fert <- modifyList(par_box, list(mfrow = c(3, 2)))
par(par_fert)
.z <- Map(bp, spec_params, spec_params_proper)

## ---- fertilization_legend ----------
par(cex = 0.6, mar = rep(0, 4))
blankplot()
legend(
  "center",
  legend = c("Not fertilized", "Fertilized"),
  col = fert_colors,
  pch = 19,
  bty = "n"
)

## ---- needle_setup ----------
needle <- results %>%
  filter(
    project == "divittorio_conifer",
    dv_needle_condition != "random"
  ) %>%
  select_if(~!all(is.na(.))) %>%
  mutate(
    needle_condition = recode_factor(
      dv_needle_condition,
      "green" = "healthy",
      "scale_insect" = "scale insect",
      "sucking_insect" = "sucking_insect",
      "winter_fleck" = "winter fleck",
      "ozone" = "ozone damage"
    )
  )

needle_colors <- c("green4", "gold", "indianred2", "springgreen", "blueviolet")
bp <- function(p, main = p, ...) {
  form <- sprintf("as.numeric(`%s`) ~ needle_condition", p)
  boxplot(as.formula(form), data = needle, outline = FALSE,
          xaxt = "n", ylab = "", main = main,
          col = needle_colors, ...)
}

## ---- needle_plot ----------
par_needle <- modifyList(par_box, list(mfrow = c(3, 2)))
par(par_needle)
.z <- Map(bp, spec_params, spec_params_proper)

## ---- needle_legend ----------
par(cex = 0.6, mar = rep(0, 4))
blankplot()
legend(
  "top",
  legend = levels(needle$needle_condition),
  col = needle_colors,
  pch = 19,
  bty = "n",
  ncol = 3
)
