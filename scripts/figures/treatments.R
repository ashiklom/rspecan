library(tidyverse)
library(rspecan)
library(metar)

variable_df <- tribble(
  ~code, ~shortname,
  "N", "# meso",
  "Cab", "Chl.",
  "Car", "Car.",
  "Cw", "Water",
  "Cm", "LDMC"
)

results <- read_csv("spectra_db/cleaned_results.csv")
metadata <- get_metadata("spectra_db")
results_raw <- results %>%
  filter(prospect_version == "D", parameter != "residual") %>%
  mutate(parameter = factor(parameter, unique(parameter))) %>%
  select(project_code, observation_id, parameter, Mean) %>%
  spread(parameter, Mean) %>%
  left_join(metadata)

treatments <- tribble(
  ~code, ~shortname,
  "treatment_temperature", "Warming",
  "treatment_water", "Drought stress",
  "dv_needle_condition",  "Needle damage",
  "bark_beetle_infested", "Bark beetle infested",
  "treatment_soy",  "Soy treatment",
  "PVY_infected", "Solanum PVY infected"
)

treatment_levels <- c(
  "green", "ozone", "scale_insect", "sucking_insect", "winter_fleck",
  "healthy", "infected",
  "C", "L", "M", "H",
  "23", "30",
  "Well-watered", "Water stressed"
)

treatment_dat <- results_raw %>%
  filter_at(treatments$code, any_vars(!is.na(.))) %>%
  filter(is.na(dv_needle_condition) | dv_needle_condition != "random") %>%
  mutate(treatment_soy = if_else(treatment_soy == "l", "L", treatment_soy)) %>%
  mutate_at(variable_df$code, ~. / mean(., na.rm = TRUE) * 100) %>%
  select(N, Cab, Car, Cw, Cm,
         species_code, !!!df2dict(treatments, "code", "shortname"))

treatment_long <- treatment_dat %>%
  gather("treatment_type", "treatment_value", treatments$shortname, na.rm = TRUE) %>%
  mutate(treatment_value = factor(treatment_value, treatment_levels))
treatment_mod <- treatment_long %>%
  group_by(treatment_type) %>%
  nest()

fit_var <- function(dat, variable) {
  if (n_distinct(dat$species_code) > 1) {
    form_string <- paste(variable, "~ treatment_value + species_code")
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
        #p.value < 0.25 ~ 0.75,
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

#debug(fit_var)

treatment_fit <- treatment_mod %>%
  mutate(mod_fit = map(data, fit_all_vars)) %>%
  unnest(mod_fit) %>%
  filter(!is.na(term))

ggplot(treatment_fit) +
  aes(x = variable, y = term, fill = factor(plevel)) +
  geom_tile() +
  geom_text(aes(label = format(estimate, digits = 1, scientific = FALSE))) +
  facet_grid(treatment_type ~ ., scales = "free_y") +
  scale_fill_brewer(type = "div") +
  theme_bw()
