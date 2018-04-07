library(tidyverse)
library(rspecan)
library(metar)
import::from(knitr, kable)
import::from(cowplot, get_legend, plot_grid)

figdir <- indir("manuscript", "figures")

results <- read_csv("spectra_db/cleaned_results.csv")
metadata <- read_csvy("spectra_db/cleaned_metadata.csvy")
species_info <- read_csvy("spectra_db/species_info.csvy")

project_colors <- read_csv("spectra_db/project_colors.csv") %>%
  df2dict("color", "short_name")

prospect_colors <- c(
  "4" = "blue",
  "5" = "orange",
  "5B" = "red",
  "D" = "purple"
)

valid_df <- tribble(
  ~specparam, ~trueparam, ~display,
  "Cab", "leaf_chltot_per_area", "Chl.",
  "Car", "leaf_cartot_per_area", "Car.",
  "Cw", "leaf_water_thickness", "Water",
  "Cm", "leaf_mass_per_area", "Dry matter"
)

md_sub <- metadata %>%
  select(
    project_code, short_name,
    observation_id,
    species_code,
    !!!valid_df$trueparam
  ) %>%
  filter_at(vars(starts_with("leaf")), any_vars(!is.na(.)))

dat <- left_join(results, md_sub) %>%
  filter(!is.na(short_name)) %>%
  # Turn off a few values for the display
  mutate(
    leaf_water_thickness = censor_if(leaf_water_thickness, leaf_water_thickness > 0.07),
    `97.5%` = censor_if(
      `97.5%`,
      parameter == "Car" & `97.5%` > 50
    )
  )

valid_data <- function(specparam, trueparam, dat) {
  trueparam_q <- rlang::sym(trueparam)
  dat %>%
    rename(observed = !!trueparam_q) %>%
    filter(parameter == specparam, !is.na(observed)) %>%
    rename(lo = `2.5%`, hi = `97.5%`) %>%
    mutate(error = abs(Mean - observed)) %>%
    select(
      project_code, short_name, species_code, prospect_version, parameter,
      Mean, SD, lo, hi, observed
    )
}

valid_plot <- function(dat) {
  ggplot(dat) +
    aes(x = Mean, xmin = lo, xmax = hi,
        y = observed, color = short_name) +
    geom_errorbarh(color = "grey50", size = 0.5, alpha = 0.5) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(mapping = aes(group = 1), method = "lm", se = FALSE,
                linetype = "solid", color = "black") +
    geom_abline(linetype = "dashed") +
    scale_color_manual(values = project_colors) +
    xlab("Inversion estimate") +
    ylab("Measured trait") +
    theme_bw()
}

fit_error <- function(nested_dat) {
  nested_dat %>%
    mutate(
      lmfit = map(data, lm, formula = observed ~ Mean),
      coefs = map(lmfit, coef),
      slope = map_dbl(coefs, "Mean"),
      intercept = map_dbl(coefs, "(Intercept)"),
      r2 = map2_dbl(lmfit, data, modelr::rsquare),
      mae = map(data, "error") %>% map_dbl(mean, na.rm = TRUE)
    )
}

fit_project_error <- function(dat) {
  by_proj <- dat %>%
    group_by(short_name, prospect_version) %>%
    nest() %>%
    fit_error()
  by_all <- dat %>%
    group_by(prospect_version) %>%
    nest() %>%
    fit_error() %>%
    mutate(project_code = "all", short_name = "Overall")
  bind_rows(by_all, by_proj) %>%
    select(
      `Project` = short_name,
      `PROSPECT version` = prospect_version,
      `Slope` = slope,
      `Intercept` = intercept,
      `R2` = r2,
      `MAE` = mae
    ) %>%
    mutate(Project = factor(Project) %>% fct_relevel("Overall"))
}

plot_project_error <- function(proj_err) {
  proj_err %>%
    mutate(`PROSPECT version` = factor(`PROSPECT version`, names(prospect_colors))) %>%
    ggplot() +
    aes(x = Project, y = R2, fill = `PROSPECT version`) +
    geom_col(position = "dodge") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_fill_manual(values = prospect_colors) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plots <- valid_df %>%
  mutate(
    data_all = map2(specparam, trueparam, valid_data, dat = dat),
    proj_err = map(data_all, fit_project_error),
    plot_proj_err = map2(
      proj_err, display,
      ~plot_project_error(.x) + ggtitle(.y) + theme(legend.position = "none")
    ),
    data_D = map(data_all, ~filter(., prospect_version == "D")),
    plots_D = map2(
      data_D, display,
      ~valid_plot(.x) + ggtitle(.y) + theme(legend.position = "none")
    )
  )

prosp_leg <- create_legend(
  prospect_colors,
  leg_title = "PROSPECT version",
  theme_bw() + theme(legend.position = "bottom")
)

png("/dev/null")
pgrid <- do.call(
  plot_grid,
  c(plots$plot_proj_err, list(nrow = 2))
)
dev.off()

pdf(infile(figdir, "project_validation_summary.pdf"))
plot_grid(pgrid, prosp_leg, nrow = 2, rel_heights = c(1, 0.1))
dev.off()

pgrid <- do.call(plot_grid, c(plots$plots_D, list(labels = letters[1:5], nrow = 2)))

leg_plt <- tibble(
  shortname = names(project_colors),
  x = seq_along(project_colors)
) %>%
  ggplot() +
  aes(x = shortname, y = x, color = shortname) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = project_colors) +
  theme(legend.position = "bottom")
leg <- get_legend(leg_plt)

pdf(infile(figdir, "prospect_D_validation.pdf"))
plot_grid(pgrid, leg, nrow = 2, rel_heights = c(1, 0.3))
dev.off()

############################################################

validate <- function(specparam, trueparam, coords_list = list()) {
  trueparam_q <- rlang::sym(trueparam)
  dat_sub <- dat %>%
    filter(parameter == specparam, !is.na(!!trueparam_q)) %>%
    rename(lo = `2.5%`, hi = `97.5%`) %>%
    mutate(error = abs(Mean - !!trueparam_q))
  plt <- ggplot(dat_sub) +
    aes_string(x = "Mean", xmin = "lo", xmax = "hi",
               y = trueparam, color = "short_name") +
    geom_point(size = 0.5) +
    geom_errorbarh(color = "grey50", size = 0.5, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(mapping = aes(group = 1), method = "lm", se = FALSE,
                linetype = "solid", color = "black") +
    geom_abline(linetype = "dashed") +
    facet_wrap(~prospect_version, scales = "free") +
    do.call(coord_cartesian, coords_list) +
    theme_bw() +
    scale_color_manual(values = project_colors)
  ggsave(infile(figdir, paste0("validation.", specparam, ".pdf")), plt)

  lm_form <- paste(trueparam, "~", "Mean")
  lm_byproject <- dat_sub %>%
    group_by(short_name, prospect_version) %>%
    nest() %>%
    mutate(
      lmfit = map(data, lm, formula = formula(lm_form)),
      coefs = map(lmfit, coef),
      slope = map_dbl(coefs, "Mean"),
      intercept = map_dbl(coefs, "(Intercept)"),
      r2 = map2_dbl(lmfit, data, modelr::rsquare),
      mae = map(data, "error") %>% map_dbl(mean, na.rm = TRUE)
    ) %>%
    select(
      `Project` = short_name,
      `PROSPECT version` = prospect_version,
      `Slope` = slope,
      `Intercept` = intercept,
      `R2` = r2,
      `MAE` = mae
    )

  kable(lm_byproject, format = "latex") %>%
    cat(file = infile(figdir, paste0("r2_byproject.", specparam, ".tex")))

  err_byspecies <- dat_sub %>%
    group_by(short_name, species_code, prospect_version) %>%
    summarize(mae = mean(error, na.rm = TRUE)) %>%
    ungroup() %>%
    select(short_name, species_code, prospect_version, mae) %>%
    mutate(
      sp_proj = interaction(species_code, short_name, prospect_version) %>%
        fct_reorder(mae)
    )

  overall_err <- dat_sub %>%
    group_by(prospect_version) %>%
    summarize(mae = mean(error, na.rm = TRUE))

  ggplot(err_byspecies) +
    aes(x = sp_proj, y = mae, fill = short_name) +
    geom_col() +
    geom_hline(aes(yintercept = mae), data = overall_err, linetype = "dashed", color = "black") +
    facet_wrap(~prospect_version, scales = "free_x") +
    xlab("Species x Project") +
    ylab("Mean absolute error") +
    scale_fill_manual(values = project_colors) +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) -> plt

  ggsave(infile(figdir, paste0("r2_speciesbyproj.", specparam, ".pdf")), plt)

  lm_bsi <- err_byspecies %>%
    left_join(species_info) %>%
    filter(!is.na(leaf_type))

  lt_colors <- c(broad = "green4", needle = "yellow2")

  plt %+% lm_bsi +
    aes(fill = leaf_type) +
    scale_fill_manual(values = lt_colors) -> plt
    #scale_fill_brewer(type = "qual") -> plt

  ggsave(infile(figdir, paste0("r2_speciesbyleaf.", specparam, ".pdf")), plt)
}

validate("Cab", "leaf_chltot_per_area")
validate("Car", "leaf_cartot_per_area")
validate("Cw", "leaf_water_thickness")
validate("Cm", "leaf_mass_per_area")
