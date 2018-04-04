library(tidyverse)
library(rspecan)

figdir <- indir("manuscript", "figures")

results <- read_csv("spectra_db/cleaned_results.csv")
metadata <- get_metadata("spectra_db")

project_colors <- read_csv("spectra_db/project_colors.csv") %>%
  df2dict("color", "short_name")

md_sub <- metadata %>%
  select(
    project_code, short_name,
    observation_id,
    leaf_chltot_per_area,
    leaf_cartot_per_area,
    leaf_water_thickness,
    leaf_mass_per_area
  ) %>%
  filter_at(vars(starts_with("leaf")), any_vars(!is.na(.)))

dat <- left_join(results, md_sub) %>%
  filter(!is.na(short_name))

valid_plot <- function(specparam, trueparam) {
  dat_sub <- filter(dat, parameter == specparam, !is.na(trueparam))
  ggplot(dat_sub) +
    aes_string(x = "Mean", y = trueparam, color = "short_name") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(mapping = aes(group = 1), method = "lm", se = FALSE,
                linetype = "dashed", color = "black") +
    facet_wrap(~prospect_version, scales = "free") +
    theme_bw() +
    scale_color_manual(values = project_colors)
}

valid_plot("Cab", "leaf_chltot_per_area")
ggsave(infile(figdir, "validation.Cab.pdf"))
valid_plot("Car", "leaf_cartot_per_area") + coord_cartesian(xlim = c(0, 40))
ggsave(infile(figdir, "validation.Car.pdf"))
valid_plot("Cw", "leaf_water_thickness") + coord_cartesian(xlim = c(0, 0.11), ylim = c(0, 0.06))
ggsave(infile(figdir, "validation.Cw.pdf"))
valid_plot("Cm", "leaf_mass_per_area") + coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 0.04))
ggsave(infile(figdir, "validation.Cm.pdf"))

############################################################

old_valid_plot <- function(dat, specparam, trueparam) {

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

