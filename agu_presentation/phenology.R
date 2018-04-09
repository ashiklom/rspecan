## ---- pheno_prep ----------
pheno <- results %>%
  filter(
    project == "yang_pheno",
    latitude == 42.531
  ) %>%
  not_missing() %>%
  mutate(
    doy = lubridate::yday(collection_date),
    species = factor(USDA_code, levels = c("BEAL2", "ACRU", "QURU"))
  ) %>%
  mutate_if(~inherits(., "units"), as.numeric)

pheno_plot <- function(dat, param, ylab = param) {
  ggplot(dat) +
    aes_string(
      x = "doy",
      y = paste0(param, "_mid"),
      ymin = paste0(param, "_lo"),
      ymax = paste0(param, "_hi")
    ) +
    geom_errorbar(color = "grey50", alpha = 0.5, size = 0.4) +
    geom_point(size = 0.4) +
    geom_smooth(size = 0.4) +
    facet_grid(USDA_code ~ ., scales = "free") +
    xlab("Day of year") +
    ylab(ylab) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      text = element_text(size = 6)
    )
}

## Lat 42.531 -- Harvard Forest
## Lat 41.362 -- Martha's Vineyard

## ---- pheno_cab ----------
pheno %>% filter(Cab_hi < 100) %>% pheno_plot("Cab", expression("Chlorophyll" ~ (mu*g ~ cm^{-2})))

## ---- pheno_car ----------
pheno %>% filter(Car_hi < 75) %>%pheno_plot("Car", expression("Carotenoids" ~ (mu*g ~ cm^{-2})))

## ---- pheno_canth ----------
pheno %>% filter(Canth_hi < 10) %>% pheno_plot("Canth", expression("Anthocyanins" ~ (mu*g ~ cm^{-2})))

## ---- pheno_cw ----------
pheno %>% filter(Cw_hi < 500) %>% pheno_plot("Cw", expression("Water" ~ (g ~ m^{-2})))

## ---- pheno_cm ----------
pheno %>% filter(Cm_hi < 100) %>% pheno_plot("Cm", expression("LDMC" ~ (g ~ m^{-2})))

## ---- validation_pheno ----------
pheno %>%
  filter(!is.na(leaf_chltot_per_area)) %>%
  ggplot() +
  aes(x = Cab_mid, xmin = Cab_lo, xmax = Cab_hi, y = leaf_chltot_per_area, color = doy) +
  geom_errorbarh(size = 0.5, color = "grey50") +
  geom_point() +
  geom_abline() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_gradient2(
    low = "blue",
    mid = "green4",
    high = "red",
    midpoint = 225)

pheno %>%
  group_by(species) %>%
  nest() %>%
  mutate(
    fit = map(data, ~lm(leaf_chltot_per_area ~ Cab_mid, data = .)),
    rsq = map2_dbl(fit, data, modelr::rsquare)
  )
