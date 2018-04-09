library(tidyverse)
library(ggforce)
dat <- readRDS("processed_results/complete_results.rds")

compare <- function(dat, compare, ...) {
  match_rxp <- "^(N|Cab|Car|Canth|Cw|Cm)_mid"
  compare_q <- rlang::sym(compare)
  dat_sub <- filter(dat, !is.na(!!compare_q))
  print(count(dat_sub, project, sort = TRUE))
  print(count(dat_sub, USDA_code, sort = TRUE))
  suppressWarnings({
    dat_sub %>%
      select(!!compare_q, matches(match_rxp)) %>%
      gather("parameter", "value", matches(match_rxp)) %>%
      ggplot() +
      aes_string(x = compare, y = "value", fill = compare, group = compare) +
      geom_boxplot() +
      facet_wrap("parameter", scales = "free")
  })
}

############################################################
# Effects
############################################################
compare(dat, "fresh_dry")
compare(dat, "treatment_water")
compare(dat, "N_fertilization_treatment")
compare(dat, "NeedleOldNew")
compare(dat, "dv_needle_condition")
compare(dat, "canopy_position")

############################################################
# No effect
############################################################
compare(dat, "PVY_infected")
compare(dat, "sun_shade")
compare(dat, "soy_stage")
compare(dat, "treatment_soy")
compare(dat, "bark_beetle_infested")
