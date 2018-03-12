source(here::here("manuscript", "setup.R"))

## ---- specific_treatments ----------
not_missing <- function(dat) select_if(dat, ~!all(is.na(.)))
dv_dat <- results_raw %>%
  filter(
    !is.na(dv_needle_condition),
    dv_needle_condition != "random"
  ) %>%
  not_missing()

dv_dat_long <- dv_dat %>%
  select(
    spectra_id, USDA_code,
    one_of(variable_df$code),#starts_with("leaf_"),
    dv_needle_condition
  ) %>%
  gather("parameter", "value", -spectra_id, -USDA_code, -dv_needle_condition)

dv_plot <- ggplot(dv_dat_long) +
  aes(x = dv_needle_condition, y = value, fill = dv_needle_condition) +
  geom_violin() +
  stat_summary(fun.data = mean_sdl, geom = "line", na.rm = TRUE) +
  facet_grid(parameter ~ USDA_code, scales = "free")

foster_dat <- results_raw %>%
  filter(!is.na(bark_beetle_infested)) %>%
  not_missing()

foster_dat_long <- foster_dat %>%
  select(one_of(variable_df$code), bark_beetle_infested, canopy_position) %>%
  gather("parameter", "value", -bark_beetle_infested, -canopy_position)

foster_plot <- ggplot(foster_dat_long) +
  aes(x = bark_beetle_infested, y = value, fill = bark_beetle_infested) +
  geom_violin() +
  facet_grid(parameter ~ ., scales = "free")
