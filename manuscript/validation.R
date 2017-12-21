source(here::here("manuscript", "setup.R"))

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

  ggplot(dat_sub) +
    aes_string(x = x, xmin = xmin, xmax = xmax, y = trueparam, color = "project") +
    geom_errorbarh(color = "grey50", size = 0.5, alpha = 0.5) +
    geom_point(size = 0.5) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(linetype = "dashed")
}

valid_setup <- function(dat, specparam, trueparam) {
  x <- paste0(specparam, "_mid")
  specparam_q <- rlang::sym(x)
  trueparam_q <- rlang::sym(trueparam)
  dat_sub <- dat %>%
    filter(!is.na(!!specparam_q), !is.na(!!trueparam_q)) %>%
    mutate_if(~inherits(., "units"), as.numeric)
  dat_sub
}

r2_plot <- function(dat, specparam, trueparam) {
  dat_sub <- valid_setup(dat, specparam, trueparam)
  x <- paste0(specparam, "_mid")
  lm_form <- as.formula(paste(trueparam, x, sep = " ~ "))
  all_fit <- lm(lm_form, data = dat_sub)
  all_r2 <- modelr::rsquare(all_fit, dat_sub)

  dat_mod <- dat_sub %>%
    group_by(project, USDA_code) %>%
    filter(n() > 5) %>%
    nest() %>%
    mutate(
      lmfit = map(data, ~lm(lm_form, data = .)),
      r2 = map2_dbl(lmfit, data, modelr::rsquare),
      x = fct_reorder(paste0(project, USDA_code), r2, .desc = TRUE)
    )
  ggplot(dat_mod) +
    aes(x = x, y = r2, fill = project) +
    geom_col() +
    #geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey30") +
    geom_hline(yintercept = all_r2, linetype = "solid", color = "black") +
    xlab("Species") +
    ylab(expression(R^2 ~ "of 1:1 fit")) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
}

noleg <- theme(legend.position = "bottom")
pcab <- valid_plot(valid_dat, "Cab", "leaf_chltot_per_area") + noleg
r2cab <- r2_plot(valid_dat, "Cab", "leaf_chltot_per_area") + noleg
pcar <- valid_plot(valid_dat, "Car", "leaf_cartot_per_area") + noleg
r2car <- r2_plot(valid_dat, "Car", "leaf_cartot_per_area") + noleg
pcw <- valid_plot(valid_dat, "Cw", "leaf_water_thickness") + coord_cartesian(ylim = c(0, 600)) + noleg
r2cw <- r2_plot(valid_dat, "Cw", "leaf_water_thickness") + noleg
pcm <- valid_plot(valid_dat, "Cm", "leaf_mass_per_area") + coord_cartesian(xlim = c(0, 1600), ylim = c(0, 400)) + noleg
r2cm <- r2_plot(valid_dat, "Cm", "leaf_mass_per_area") + noleg
plot_grid(
  pcab, r2cab,
  pcar, r2car,
  pcw, r2cw,
  pcm, r2cm,
  nrow = 4, ncol = 2, labels = letters[1:8])
