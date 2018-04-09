## ---- validation_setup ----------
library(gridExtra)
project_color_vec <- project_colors$project_color
names(project_color_vec) <- project_colors$long_project

theme_big <- theme(
  text = element_text(size = 7)
)

theme_small <- theme(
  text = element_text(size = 4)
)

valid_setup <- function(dat, specparam, trueparam) {
  x <- paste0(specparam, "_mid")
  specparam_q <- rlang::sym(x)
  trueparam_q <- rlang::sym(trueparam)
  dat_sub <- dat %>%
    filter(!is.na(!!specparam_q), !is.na(!!trueparam_q)) %>%
    mutate_if(~inherits(., "units"), as.numeric)
  dat_sub
}

valid_plot <- function(dat, specparam, trueparam) {
  dat_sub <- valid_setup(dat, specparam, trueparam)
  x <- paste0(specparam, "_mid")
  xmin <- paste0(specparam, "_lo")
  xmax <- paste0(specparam, "_hi")
  lm_form <- as.formula(paste(trueparam, x, sep = " ~ "))

  ggplot(dat_sub) +
    aes_string(x = x, xmin = xmin, xmax = xmax, y = trueparam, color = "long_project") +
    geom_errorbarh(color = "grey50", size = 0.3, alpha = 0.5) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_abline(linetype = "dashed") +
    scale_color_manual(values = project_color_vec) +
    xlab("Spectral estimate") +
    ylab("Measured value") +
    guides(color = FALSE) +
    theme_bw() +
    theme(
      panel.grid = element_blank()
    )
}

r2_plot <- function(dat, specparam, trueparam) {
  dat_sub <- valid_setup(dat, specparam, trueparam)
  x <- paste0(specparam, "_mid")
  lm_form <- as.formula(paste(trueparam, x, sep = " ~ "))
  dat_mod <- dat_sub %>%
    group_by(long_project, USDA_code) %>%
    filter(n() > 5) %>%
    nest() %>%
    mutate(
      lmfit = map(data, ~lm(lm_form, data = .)),
      r2 = map2_dbl(lmfit, data, modelr::rsquare),
      x = fct_reorder(paste0(long_project, USDA_code), r2, .desc = TRUE)
    )
  ggplot(dat_mod) +
    aes(x = x, y = r2, fill = long_project) +
    geom_col() +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    guides(fill = FALSE) +
    xlab("Species") +
    ylab(expression(R^2 ~ "of 1:1 fit")) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_fill_manual(values = project_color_vec)
}

y_lab <- 0.85
r2_chl_lab <- annotate("text", 45, y_lab, label = "Chl")
r2_car_lab <- annotate("text", 26, y_lab, label = "Car")
r2_cw_lab <- annotate("text", 70, y_lab, label = "Water")
r2_cm_lab <- annotate("text", 105, y_lab, label = "LDMC")

## ---- validation_chl ----------
valid_plot(results, "Cab", "leaf_chltot_per_area") +
  theme_big +
  annotate("text", x = 17, y = 115, label = '"Chl" ~ (mu*g~cm^{-2})', parse = TRUE)

## ---- validation_water ----------
valid_plot(results, "Cw", "leaf_water_thickness") +
  coord_cartesian(ylim = c(0, 600))

## ---- r2_chl ----------
r2_plot(results, "Cab", "leaf_chltot_per_area") +
  theme_big +
  annotate("text", 40, 0.95, label = "Chl")

## ---- r2_all ----------
pcab <- r2_plot(results, "Cab", "leaf_chltot_per_area") + theme_small + r2_chl_lab
pcar <- r2_plot(results, "Car", "leaf_cartot_per_area") + theme_small + r2_car_lab
pcw <- r2_plot(results, "Cw", "leaf_water_thickness") + theme_small + r2_cw_lab
pcm <- r2_plot(results, "Cm", "leaf_mass_per_area") + theme_small + r2_cm_lab
grid.arrange(arrangeGrob(pcab, pcar, pcw, pcm, nrow = 2))
