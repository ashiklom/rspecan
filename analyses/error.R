library(tidyverse)
library(ggforce)
dat <- readRDS("processed_results/complete_results.rds")

dat_chl <- dat %>%
  filter(
    !is.na(leaf_chltot_per_area),
    !is.na(Cab_mid)
  )

fitlm <- function(x, y) {
  tryCatch({
    lm(y ~ x)
  }, error = function(e) NULL)
}

mod_chl <- dat_chl %>%
  group_by(USDA_code, project) %>%
  filter(n() > 3) %>%
  nest() %>%
  mutate(
    lmfit = map(data, ~lm(leaf_chltot_per_area ~ Cab_mid, data = .)),
    r2 = map2_dbl(lmfit, data, modelr::rsquare)
  )

mod_chl %>%
  filter(r2 < 1) %>%
  mutate(
    USDA_code = fct_reorder(USDA_code, r2, fun = "mean", .desc = TRUE)
  ) %>%
ggplot() +
  aes(x = USDA_code, y = r2, fill = project) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1")

mod_chl %>% select(r2, USDA_code, group, everything()) %>% arrange(desc(r2))

dat_chl2 <- dat_chl %>%
  mutate(
    chl_abserror = Cab_mid - leaf_chltot_per_area,
    chl_relerror = chl_abserror / leaf_chltot_per_area
  )

chl2_vars <- c(
  "leaf_N_pct_mass", "leaf_fiber_pct_mass", "leaf_lignin_pct_mass", 
  "leaf_mass_per_area", "leaf_chltot_per_area", "leaf_chla_per_area",
  "leaf_chlb_per_area", "leaf_protein_pct_mass", "leaf_water_pct_mass"
)
chl2_vars <- c(
  "Canth_mid", "Car_mid", "Cbrown_mid", "Cm_mid", "Cw_mid", "N_mid", "residual_mid"
)
chl2_formula <- paste(chl2_vars, collapse = " + ") %>%
  paste("chl_relerror ~", .) %>%
  as.formula()
chl2_model <- lm(chl2_formula, data = dat_chl2)

# Relative error in Chl ~ chl
zero <- set_units(0, "ug cm-2")
ggplot(dat_chl2 %>% filter(leaf_chlb_per_area > zero)) +
  aes(y = chl_relerror, x = Cw_mid, color = project) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-1, 3))
