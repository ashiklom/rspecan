library(tidyverse)
library(ggforce)
dat <- readRDS("processed_results/complete_results.rds")

valid_plot <- function(dat, specparam, trueparam) {

  x <- paste0(specparam, "_mid")
  xmin <- paste0(specparam, "_lo")
  xmax <- paste0(specparam, "_hi")

  specparam_q <- rlang::sym(x)
  trueparam_q <- rlang::sym(trueparam)
  dat_sub <- dat %>%
    filter(!is.na(!!specparam_q), !is.na(!!trueparam_q))

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

  ggplot(dat_sub) +
    aes_string(x = x, xmin = xmin, xmax = xmax, y = trueparam, color = "project") +
    geom_errorbarh(color = "grey50", size = 0.5, alpha = 0.5) +
    geom_point(size = 0.5) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(linetype = "dashed") +
    annotate("text", x = Inf, y = Inf, label = lab, hjust = 1, vjust = 1)
}

valid_plot(dat, "Cab", "leaf_chltot_per_area")
valid_plot(dat, "Car", "leaf_cartot_per_area")
valid_plot(dat, "Cw", "leaf_water_thickness") + coord_cartesian(ylim = c(0, 600))
valid_plot(dat, "Cm", "leaf_mass_per_area") + coord_cartesian(xlim = c(0, 1600), ylim = c(0, 400))
