source(here::here("manuscript", "setup.R"))

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
