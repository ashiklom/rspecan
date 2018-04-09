library(tidyverse)
library(ggforce)
dat <- readRDS("processed_results/complete_results.rds")

match_rxp <- "^(N|Cab|Car|Canth|Cw|Cm)_mid"

dat_sub <- dat %>%
  filter(!is.na(treatment_water), !is.na(treatment_temperature)) %>%
  mutate(
    treatment_water = factor(treatment_water, c("Well-watered", "Water stressed")),
    treatment_temperature = factor(treatment_temperature, c(23, 30))
  ) %>%
  unite(treatment, treatment_temperature, treatment_water, remove = FALSE) %>%
  select_if(~!all(is.na(.)))

dat_long <- dat_sub %>%
  select(treatment, matches(match_rxp)) %>%
  gather(parameter, value, -treatment)

bp <- function(p) {
  form <- sprintf("as.numeric(%s) ~ treatment", p)
  boxplot(as.formula(form), data = dat_sub, outline = FALSE, ylab = p)
}

traits <- c("N_mid", "Cab_mid", "Car_mid", "Canth_mid", "Cw_mid", "Cm_mid")
traits <- dat_sub %>%
  select_if(is.double) %>%
  select(-ends_with("_lo"), -ends_with("_hi")) %>%
  colnames()
i <- imguR::imgur("png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(5, 4))
lapply(traits, bp)
imguR::imgur_off(i)$link

sp <- readRDS("processed_data/ecosis_milkweed_stress.rds")$spectra
ww_ids <- dat_sub %>%
  filter(treatment_water == "Well-watered") %>%
  pull(spectra_id)
ws_ids <- dat_sub %>%
  filter(treatment_water == "Water stressed") %>%
  pull(spectra_id)
ww_spec <- sp[, ww_ids]
ws_spec <- sp[, ws_ids]
t_col <- function(x, alpha) {
  do.call(rgb, as.list(c(col2rgb(x)[, 1], alpha = alpha, max = 255)))
}
i <- imguR::imgur("png")
wl <- 350:2500
matplot(wl, ws_spec, type = "l", col = t_col("brown", 120), xlim = c(400, 725))
matplot(wl, ww_spec, type = "l", col = t_col("deepskyblue", 40), xlim = c(400, 725), add = TRUE)
imguR::imgur_off(i)$link
