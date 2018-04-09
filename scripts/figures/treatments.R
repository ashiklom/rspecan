library(tidyverse)
library(rspecan)
library(metar)

results <- read_csv("spectra_db/cleaned_results.csv")
metadata <- read_csvy("spectra_db/cleaned_metadata.csvy")
results_raw <- results %>%
  filter(prospect_version == "D", parameter != "residual") %>%
  mutate(parameter = factor(parameter, unique(parameter))) %>%
  select(project_code, observation_id, parameter, Mean) %>%
  spread(parameter, Mean) %>%
  left_join(metadata) %>%
  mutate_if(is.character, na_if, "")

treatments <- tribble(
  ~code, ~shortname,
  "climate", "Climate",
  "sun_shade", "Shade leaf",
  "barnes_2017", "Barnes 2017",
  "treatment_temperature", "Milkweed - Warming",
  "treatment_water", "Milkweed - Drought",
  "dv_needle_condition",  "Needle damage",
  "treatment_soy",  "Soy treatment",
  "PVY_infected", "Potato virus"
)

tsub <- filter(treatments, code != "barnes_2017", code != "climate")

treatment_levels <- tribble(
  ~shortname, ~tcode, ~tshortname,
  "Needle damage", "green", "Green",
  "Needle damage", "ozone", "Ozone",
  "Needle damage", "scale_insect", "Scale insect",
  "Needle damage", "sucking_insect", "Sucking insect",
  "Needle damage", "winter_fleck", "Winter fleck",
  "Potato virus", "healthy", "Healthy",
  "Potato virus", "infected", "Infected",
  "Soy treatment", "C", "Control",
  "Soy treatment", "L", "Low",
  "Soy treatment", "M", "Medium",
  "Soy treatment", "H", "High",
  "Milkweed - Warming", "23", "Normal temperature",
  "Milkweed - Warming", "30", "Warmer",
  "Milkweed - Drought", "Well-watered", "Well-watered",
  "Milkweed - Drought", "Water stressed", "Water stressed",
  "Shade leaf", "sun", "Sun",
  "Shade leaf", "shade", "Shade",
  "Barnes 2017", "air_temperature", "Air temp.",
  "Barnes 2017", "leaf_temperature", "Leaf temp.",
  "Barnes 2017", "vapor_pressure_deficit", "VPD",
  "Climate", "AMT", "Annual mean temp.",
  "Climate", "AP", "Annual precip."
)

normalize <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  (x - s) / m
}

format_treat <- function(fitdf) {
  mutate(
    fitdf,
    plevel = sign(estimate) * case_when(
      p.value < 0.025 ~ 0.95,
      p.value < 0.05 ~ 0.9,
      TRUE ~ 0),
    estimate = round(estimate, 3),
  )
}

treatment_dat <- results_raw %>%
  filter_at(tsub$code, any_vars(!is.na(.))) %>%
  filter(is.na(dv_needle_condition) | dv_needle_condition != "random") %>%
  mutate(treatment_soy = if_else(treatment_soy == "l", "L", treatment_soy)) %>%
  mutate_at(variable_df$code, normalize) %>%
  select(
    !!!variable_df$code,
    species_code,
    tsub$code
  )

treatment_long <- treatment_dat %>%
  gather("treatment_type", "treatment_value", tsub$code, na.rm = TRUE) %>%
  mutate(treatment_value = factor(treatment_value, treatment_levels$tcode))

treatment_mod <- treatment_long %>%
  group_by(treatment_type) %>%
  nest()

fit_var <- function(dat, variable) {
  dat <- filter(dat, !is.na(treatment_value))
  if (n_distinct(dat$species_code) > 1) {
    form_string <- paste(variable, "~ treatment_value + species_code")
  } else {
    form_string <- paste(variable, "~ treatment_value")
  }
  fit <- lm(formula(form_string), data = dat)
  broom::tidy(fit) %>%
    filter(term != "(Intercept)", !grepl("species_code", term)) %>%
    mutate(
      variable = !!variable,
      term = gsub("treatment_value", "", term)
    ) %>%
    format_treat()
}

fit_all_vars <- function(dat) {
  dat$treatment_value <- fct_drop(dat$treatment_value)
  variables <- variable_df[["code"]]
  outlist <- purrr::map(variables, ~fit_var(dat, .))
  Reduce(bind_rows, outlist)
}

treatment_fit <- treatment_mod %>%
  mutate(mod_fit = map(data, fit_all_vars)) %>%
  unnest(mod_fit) %>%
  filter(!is.na(term))

barnes_fit <- results_raw %>%
  filter(project_code == "barnes_2017") %>%
  mutate_if(is.character, na_if, "") %>%
  select_if(~!all(is.na(.)))

barnes_mod <- barnes_fit %>%
  select(
    !!!variable_df$code,
    leaf_temperature,
    air_temperature,
    delta_temperature,
    vapor_pressure_deficit
  ) %>%
  gather(variable, value, !!!variable_df$code) %>%
  group_by(variable) %>%
  mutate(norm_value = normalize(value)) %>%
  nest() %>%
  mutate(
    lmfit = map(data, lm, formula = formula(value ~ leaf_temperature + air_temperature + vapor_pressure_deficit)),
    lmtidy = map(lmfit, ~broom::tidy(.) %>% filter(term != "(Intercept)"))
  ) %>%
  unnest(lmtidy) %>%
  mutate(treatment_type = "barnes_2017") %>%
  format_treat()

multi_site_spp <- results_raw %>%
  filter(!is.na(species_code)) %>%
  group_by(species_code) %>%
  summarize(nsite = n_distinct(latitude, longitude)) %>%
  arrange(desc(nsite)) %>%
  filter(nsite > 10)

clim_dat <- readRDS("extdata/worldclim_met.rds") %>%
  select(-site_name) %>%
  unnest(met_data) %>%
  select(-ends_with("1"))

multi_site_dat <- results_raw %>%
  semi_join(multi_site_spp) %>%
  select(project_code, short_name, observation_id, !!!variable_df$code,
         latitude, longitude, species_code) %>%
  mutate_at(variable_df$code, normalize) %>%
  left_join(clim_dat)

clim_nest <- multi_site_dat %>%
  gather(variable, value, !!!variable_df$code) %>%
  group_by(variable) %>%
  nest()

clim_mod <- clim_nest %>%
  mutate(
    lmfit = map(data, lm, formula = formula(value ~ species_code + AP + AMT)),
    lmtidy = map(lmfit, broom::tidy)
  )

clim_tidy <- clim_mod %>%
  unnest(lmtidy) %>%
  filter(term != "(Intercept)", !grepl("species_code", term)) %>%
  mutate(treatment_type = "climate") %>%
  format_treat()

treatment_plot <- bind_rows(treatment_fit, barnes_mod, clim_tidy) %>%
  mutate(
    term = factor(term, treatment_levels$tcode) %>%
      lvls_revalue(treatment_levels$tshortname),
    variable = factor(variable, variable_df$code) %>%
      lvls_revalue(variable_df$shortname),
    treatment_type = factor(treatment_type, treatments$code) %>%
      lvls_revalue(treatments$shortname)
  )

plt <- ggplot(treatment_plot) +
  aes(x = variable, y = term, fill = factor(plevel)) +
  geom_tile() +
  geom_text(aes(label = format(estimate, digits = 1, scientific = FALSE))) +
  facet_grid(treatment_type ~ ., scales = "free_y") +
  scale_fill_brewer(type = "div") +
  labs(fill = "Significance level") +
  theme_bw() +
  theme(axis.title = element_blank())

ggsave(infile("manuscript/figures/treatment_summary.pdf"), plt)
