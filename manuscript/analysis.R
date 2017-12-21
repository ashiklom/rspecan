## ---- setup ----------
library(knitr)
library(rspecan)
library(cowplot)
spectra_file <- here::here("processed_data", "spectra.h5")
results_file <- here::here("processed_results", "complete_results.rds")
opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  cache = 2
)
set_alias(w = "fig.width", h = "fig.height")
results_raw <- readRDS(results_file) %>%
  filter(!(target_type %in% c("npv", "rock", "soil")))
variable_df <- tribble(
  ~code, ~shortname, ~longname, ~name_with_unit,
  "N_mid", "# meso", "Number of mesophyll layers", "Number of mesophyll layers",
  "Cab_mid", "Chl.", "Chlorophyll", "Chlorophyll ~ (mu * g ~ cm ^ {-2})",
  "Car_mid", "Car.", "Carotenoids", "Carotenoids ~ (mu * g ~ cm ^ {-2})",
  "Canth_mid", "Anth.", "Anthocyanins", "Anthocyanins ~ (mu * g ~ cm ^ {-2})",
  "Cw_mid", "Water", "Water", "Water ~ (g ~ m ^ {-2})",
  "Cm_mid", "LDMC", "Dry matter", "Dry matter ~ (g ~ m ^ {-2})"
)

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

## ---- treatments ----------
treatment_cols <- c(
  "N_fertilization_treatment",
  "treatment_temperature",
  "treatment_water",
  "dv_needle_condition",
  "bark_beetle_infested",
  "treatment_soy",
  "PVY_infected"
)
treatment_levels <- c(
  "no", "yes",
  "green", "ozone", "scale_insect", "sucking_insect", "winter_fleck",
  "Not Fertalized", "Fertalized",
  "healthy", "infected",
  "C", "L", "M", "H",
  "23", "30",
  "Well-watered", "Water stressed"
)
treatment_dat <- results_raw %>%
  filter_at(treatment_cols, any_vars(!is.na(.))) %>%
  filter(is.na(dv_needle_condition) | dv_needle_condition != "random") %>%
  mutate(treatment_soy = if_else(treatment_soy == "l", "L", treatment_soy)) %>%
  select(N_mid, Cab_mid, Car_mid, Canth_mid, Cw_mid, Cm_mid,
         USDA_code, one_of(treatment_cols))

treatment_long <- treatment_dat %>%
  gather("treatment_type", "treatment_value", treatment_cols, na.rm = TRUE) %>%
  mutate(treatment_value = factor(treatment_value, treatment_levels))
treatment_mod <- treatment_long %>%
  group_by(treatment_type) %>%
  nest()

fit_var <- function(dat, variable) {
  if (n_distinct(dat$USDA_code) > 1) {
    form_string <- paste(variable, "~ treatment_value + USDA_code")
  } else {
    form_string <- paste(variable, "~ treatment_value")
  }
  fit <- lm(formula(form_string), data = dat)
  broom::tidy(fit) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      variable = !!variable,
      term = gsub("treatment_value", "", term),
      plevel := sign(estimate) * case_when(
        p.value < 0.05 ~ 0.95,
        p.value < 0.1 ~ 0.9,
        p.value < 0.25 ~ 0.75,
        TRUE ~ 0
      )
    )
}

fit_all_vars <- function(dat) {
  dat$treatment_value <- fct_drop(dat$treatment_value)
  variables <- variable_df[["code"]]
  outlist <- purrr::map(variables, ~fit_var(dat, .))
  Reduce(bind_rows, outlist) %>%
    mutate(
      variable = factor(variable, variables) %>%
        lvls_revalue(variable_df[["shortname"]]),
      term = factor(term, treatment_levels)
    )
}

treatment_fit <- treatment_mod %>%
  mutate(mod_fit = map(data, fit_all_vars)) %>%
  unnest(mod_fit) %>%
  filter(!is.na(term))

ggplot(treatment_fit) +
  aes(x = variable, y = term, fill = factor(plevel)) +
  geom_tile() +
  geom_text(aes(label = format(estimate, digits = 1, scientific = FALSE))) +
  facet_grid(treatment_type ~ ., scales = "free_y") +
  scale_fill_brewer(type = "div")

## ---- validation ----------
validate_df <- tribble(
  ~code, ~obs_code,
  "Cab_mid", "leaf_chltot_per_area",
  "Car_mid", "leaf_cartot_per_area",
  "Cw_mid", "leaf_water_thickness",
  "Cm_mid", "leaf_mass_per_area"
)
valid_dat <- results_raw
valid_plot <- function(dat, specparam, trueparam) {

  x <- paste0(specparam, "_mid")
  xmin <- paste0(specparam, "_lo")
  xmax <- paste0(specparam, "_hi")

  specparam_q <- rlang::sym(x)
  trueparam_q <- rlang::sym(trueparam)
  dat_sub <- dat %>%
    filter(!is.na(!!specparam_q), !is.na(!!trueparam_q)) %>%
    mutate_at(c(x, xmin, xmax, trueparam), as.numeric)

  lm_form <- as.formula(paste(trueparam, x, sep = " ~ "))
  dat_mod <- dat_sub %>%
    group_by(project) %>%
    nest() %>%
    mutate(
      lmfit = map(data, ~lm(lm_form, data = .)),
      r2 = map2_dbl(lmfit, data, modelr::rsquare),
      label = map2_chr(project, r2, ~sprintf("%s: %.02f", .x, .y))
    ) %>%
    arrange(desc(r2))

  lab <- paste(dat_mod$label, collapse = "\n")

  all_fit <- lm(lm_form, data = dat_sub)
  all_lab <- sprintf("Overall: %02f", modelr::rsquare(all_fit, dat_sub))

  ggplot(dat_sub) +
    aes_string(x = x, xmin = xmin, xmax = xmax, y = trueparam, color = "project") +
    geom_errorbarh(color = "grey50", size = 0.5, alpha = 0.5) +
    geom_point(size = 0.5) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(linetype = "dashed") +
    annotate("text", x = Inf, y = Inf, label = lab, hjust = 1, vjust = 1) +
    annotate("text", x = Inf, y = 0, label = all_lab, hjust = 1, vjust = 1)
}

noleg <- theme(legend.position = "bottom")
pcab <- valid_plot(valid_dat, "Cab", "leaf_chltot_per_area") + noleg
pcar <- valid_plot(valid_dat, "Car", "leaf_cartot_per_area") + noleg
pcw <- valid_plot(valid_dat, "Cw", "leaf_water_thickness") + coord_cartesian(ylim = c(0, 600)) + noleg
pcm <- valid_plot(valid_dat, "Cm", "leaf_mass_per_area") + coord_cartesian(xlim = c(0, 1600), ylim = c(0, 400)) + noleg
plot_grid(pcab, pcar, pcw, pcm, nrow = 2, ncol = 2, labels = c("a", "b", "c", "d"))
