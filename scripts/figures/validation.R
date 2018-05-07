library(tidyverse)
library(rspecan)
library(metar)
library(GGally)
import::from(knitr, kable)
import::from(cowplot, get_legend, plot_grid)
import::from(viridis, scale_color_viridis, scale_fill_viridis)
import::from(MASS, rlm)

theme_set(theme_bw())

figdir <- indir("manuscript", "figures")

results <- read_csv("spectra_db/cleaned_results.csv")
metadata <- read_csvy("spectra_db/cleaned_metadata.csvy")
species_info <- read_csv(
  "spectra_db/species_info.csvy",
  col_types = cols(.default = "c", nitrogen_fixer = "l", try_species_ID = "i",
                   myco_is_am = "l", is_shade_intolerant = "l")
)

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
    instrument_code,
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
    dplyr::rename(observed = !!trueparam_q, lo = `2.5%`, hi = `97.5%`) %>%
    dplyr::filter(parameter == specparam, !is.na(observed)) %>%
    dplyr::mutate(
      error = Mean - observed,
      error2 = error ^ 2,
      errora = abs(error),
      rel_error = error / observed,
      rel_error2 = rel_error ^ 2,
      rel_errora = abs(rel_error)
    ) #%>%
    #dplyr::select(
      #project_code, short_name, species_code, prospect_version, parameter,
      #Mean, SD, lo, hi, observed
    #)
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

dat_pd <- dat %>% filter(prospect_version == "D")
dat_valid <- valid_df %>%
  mutate(data = map2(specparam, trueparam, valid_data, dat = dat_pd)) %>%
  unnest(data) %>%
  left_join(species_info) %>%
  filter(
    !(parameter == "Car" & Mean > 30),
    growth_form != "vine"
  ) %>%
  mutate(
    instrument_code = factor(instrument_code),
    short_name = factor(short_name, names(project_colors)) %>% fct_drop(),
    growth_form = factor(growth_form) %>%
      fct_collapse(woody = c("shrub", "tree"))
  )

plot_bygf <- function(param) {
  dat_valid %>%
    filter(parameter == !!param, !is.na(growth_form), growth_form != "vine") %>%
    ggplot() +
    aes(x = Mean, xmin = lo, xmax = hi, y = observed, color = short_name) +
    geom_errorbarh(color = "grey60", alpha = 0.5) +
    geom_point(size = 0.5, alpha = 0.7) +
    geom_smooth(method = MASS::rlm, se = FALSE) +
    geom_smooth(aes(group = 1), method = MASS::rlm, se = FALSE, color = "black") +
    geom_abline(linetype = "dashed") +
    facet_grid(~ growth_form, drop = FALSE) +
    scale_color_manual(values = project_colors, drop = FALSE) +
    labs(color = "Project")
}
plist <- map(c("Cab", "Car", "Cw", "Cm"), plot_bygf)
pltbygf <- ggmatrix(
  plist, nrow = 4, ncol = 1,
  yAxisLabels = c("Chl.", "Car.", "Water", "LMA"),
  legend = c(4, 1)
) +
  labs(x = "Inversion estimate", y = "Observed") +
  theme_grey() +
  theme(legend.position = "bottom")
ggsave("manuscript/figures/validation_by_gf.pdf", pltbygf, width = 8, height = 8)

############################################################

specparam <- "Cab"
trueparam <- "leaf_chltot_per_area"
coords_list <- list()

chl_dat <- valid_data(specparam, trueparam, dat)

valid_subset <- function(specparam, trueparam) {
  dat %>%
    filter(parameter == specparam, !is.na(!!trueparam_q)) %>%
    rename(lo = `2.5%`, hi = `97.5%`) %>%
    mutate(error = abs(Mean - !!trueparam_q))
}

chl_dat <- valid_subset("Cab", "leaf_chltot_per_area")

validate <- function(specparam, trueparam, coords_list = list()) {
  trueparam_q <- rlang::sym(trueparam)
  dat_sub <- valid_data(specparam, trueparam, dat)
  plt <- ggplot(dat_sub) +
    aes_string(x = "Mean", xmin = "lo", xmax = "hi",
               y = "observed", color = "short_name") +
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

  dat_mod <- dat_sub %>%
    filter(grepl("Cedar Creek", short_name), prospect_version == "D") %>%
    group_by(species_code) %>%
    nest() %>%
    mutate(
      fit = map(data, lm, formula = observed ~ Mean),
      r2 = map2_dbl(fit, data, modelr::rsquare)
    ) %>%
    select(species_code, r2)

  dat_sub %>%
    filter(grepl("Cedar Creek", short_name), prospect_version == "D") %>%
    left_join(dat_mod, by = "species_code") %>%
    mutate(species_code = fct_reorder(species_code, r2, .desc = TRUE)) %>%
    ggplot() +
    aes(x = Mean, y = observed, group = species_code) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(linetype = "dashed") +
    facet_wrap(~species_code)

  plt %+%
    (dat_sub %>% filter(grepl("Cedar Creek", short_name))) %+%
    aes(color = species_code) %+%
    scale_color_brewer(type = "qual")
  ggsave(infile(figdir, paste0("validation_", specparam, ".pdf")), plt)

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

  kable(lm_byproject, format = "latex", booktabs = TRUE) %>%
    cat(file = infile(figdir, paste0("r2_byproject_", specparam, ".tex")))

  err_byspecies <- dat_sub %>%
    group_by(short_name, species_code, prospect_version) %>%
    filter(n() > 5) %>%
    nest() %>%
    mutate(
      N = map_dbl(data, nrow),
      lmfit = map(data, lm, formula = formula(lm_form)),
      coefs = map(lmfit, coef),
      slope = map_dbl(coefs, "Mean"),
      intercept = map_dbl(coefs, "(Intercept)"),
      r2 = map2_dbl(lmfit, data, modelr::rsquare),
      mae = map(data, "error") %>% map_dbl(mean, na.rm = TRUE)
    ) %>%
    select(short_name, species_code, prospect_version, slope, intercept, r2, mae, N) %>%
    mutate(
      sp_proj = interaction(species_code, short_name, prospect_version) %>%
        fct_reorder(r2, .desc = TRUE)
    )
    #arrange(sp_proj) %>%
    #mutate(
      #xhi = cumsum(N),
      #xlo = xhi - N,
      #x = ((xlo + xhi) / 2) ^ (1/2),
      #wid = (N) ^ (1/3)
    #)

  #overall_err <- dat_sub %>%
    #group_by(prospect_version) %>%
    #summarize(mae = mean(error, na.rm = TRUE))

  ggplot(err_byspecies) +
    aes(x = sp_proj, y = r2, fill = short_name) +
    geom_col(position = "identity") +
    #geom_hline(aes(yintercept = mae), data = overall_err, linetype = "dashed", color = "black") +
    facet_wrap(~prospect_version, scales = "free_x") +
    xlab("Species x Project") +
    ylab("Mean absolute error") +
    scale_fill_manual(values = project_colors) +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) -> plt

  ggsave(infile(figdir, paste0("r2_speciesbyproj_", specparam, ".pdf")), plt)

  lm_bsi <- err_byspecies %>%
    left_join(species_info) %>%
    filter(!is.na(leaf_type))

  lt_colors <- c(broad = "green4", needle = "yellow2")

  plt %+% lm_bsi +
    aes(fill = leaf_type) +
    scale_fill_manual(values = lt_colors) -> plt
    #scale_fill_brewer(type = "qual") -> plt

  ggsave(infile(figdir, paste0("r2_speciesbyleaf_", specparam, ".pdf")), plt)
}

validate("Cab", "leaf_chltot_per_area")
validate("Car", "leaf_cartot_per_area")
validate("Cw", "leaf_water_thickness")
validate("Cm", "leaf_mass_per_area")
