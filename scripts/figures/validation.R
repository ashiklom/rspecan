library(tidyverse)
library(rspecan)
library(metar)
library(GGally)
library(ggridges)
import::from(knitr, kable)
import::from(cowplot, get_legend, plot_grid)
import::from(viridis, scale_color_viridis, scale_fill_viridis)
import::from(MASS, rlm)

options(na.action = na.omit, error = recover)

theme_set(theme_bw())

figdir <- indir("manuscript", "figures")

results <- read_csv("spectra_db/cleaned_results.csv")
metadata <- read_csvy("spectra_db/cleaned_metadata.csvy")

species_info <- read_csv(
  "spectra_db/species_info.csvy",
  col_types = cols(.default = "c", nitrogen_fixer = "l", try_species_ID = "i",
                   myco_is_am = "l", is_shade_intolerant = "l")
) %>%
  mutate(
    gf_lt = interaction(growth_form, leaf_type, drop = TRUE) %>%
      fct_recode(
        "grass" = "graminoid.broad",
        "herb" = "herb.broad",
        "broadleaf" = "shrub.broad",
        "broadleaf" = "tree.broad",
        "conifer" = "tree.needle"
      )
  )

project_colors <- read_csv("spectra_db/project_colors.csv") %>%
  df2dict("color", "short_name")
project_fill <- scale_fill_manual(values = project_colors)

gflt_fill <- scale_fill_manual(values = c(broadleaf = "green4", conifer = "blue4",
                                          herb = "purple", grass = "orange"))
gflt_color <- scale_color_manual(values = c(broadleaf = "green4", conifer = "blue4",
                                            herb = "purple", grass = "orange"))

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
  anti_join(species_info %>% filter(growth_form == "vine")) %>%
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
    )
}

valid_plot <- function(dat) {
  ggplot(dat) +
    aes(x = Mean, xmin = lo, xmax = hi,
        y = observed, color = short_name) +
    geom_errorbarh(color = "grey50", size = 0.5, alpha = 0.5) +
    geom_point(size = 0.5) +
    geom_smooth(method = MASS::rlm, se = FALSE) +
    geom_smooth(mapping = aes(group = 1), method = MASS::rlm, se = FALSE,
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
      lmfit = map(data, ~MASS::rlm(observed ~ Mean, data = .)),
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
    gf_lt = fct_drop(gf_lt)
  )

message("Validation by param and growth form")
plot_bygf <- function(param) {
  dat_valid %>%
    filter(parameter == !!param, !is.na(gf_lt)) %>%
    ggplot() +
    aes(x = Mean, xmin = lo, xmax = hi, y = observed, color = short_name) +
    geom_errorbarh(color = "grey60", alpha = 0.5) +
    geom_point(size = 0.5, alpha = 0.7) +
    geom_smooth(method = MASS::rlm, se = FALSE) +
    geom_smooth(aes(group = 1), method = MASS::rlm, se = FALSE, color = "black") +
    geom_abline(linetype = "dashed") +
    facet_grid(~ gf_lt, drop = FALSE) +
    scale_color_manual(values = project_colors, drop = FALSE) +
    labs(color = "Project")
}
plist <- map(c("Cab", "Car", "Cw", "Cm"), plot_bygf)
pltbygf <- ggmatrix(
  plist, nrow = 4, ncol = 1,
  labeller = label_parsed,
  yAxisLabels = c(
    '"Chl." ~ (mu * g ~ cm ^ {-2})',
    '"Car." ~ (mu * g ~ cm ^ {-2})',
    '"Water" ~ (g ~ cm ^ {-2})',
    '"LMA" ~ (g ~ cm ^ {-2})'
  ),
  legend = c(4, 1)
) +
  labs(x = "Inversion estimate", y = "Observed") +
  theme_grey() +
  theme(legend.position = "bottom")
ggsave("manuscript/figures/validation_by_gf.pdf", pltbygf, width = 8, height = 8)

############################################################
do_r2 <- function(x, y) {
  fit <- MASS::rlm(y ~ x)
  d <- tibble(x = x, y = y)
  modelr::rsquare(fit, d)
}

error_model <- dat_valid %>%
  mutate(
    specparam = recode_factor(
      specparam,
      Cab = "\"Chl.\" ~ (mu * g ~ cm ^ {-2})",
      Car = "\"Car.\" ~ (mu * g ~ cm ^ {-2})",
      Cw = "\"Water\" ~ (g ~ cm ^ {-2})",
      Cm = "\"LMA\" ~ (g ~ cm ^ {-2})"
    )
  ) %>%
  group_by(specparam, species_code, short_name) %>%
  summarize(
    N = n(),
    r2 = possibly(do_r2, NA_real_)(Mean, observed),
    rmse = sqrt(mean((Mean - observed) ^ 2, na.rm = TRUE)),
    rmse_rel = sqrt(mean((Mean/observed - 1) ^ 2, na.rm = TRUE))
  ) %>%
  ungroup()

proj_pft_plot <- dat_valid %>%
  group_by(species_code, short_name, specparam) %>%
  ungroup() %>%
  mutate(
    pct_error = 100 * (error/observed),
    specparam = recode_factor(
      specparam,
      Cab = "\"Chl.\" ~ (mu * g ~ cm ^ {-2})",
      Car = "\"Car.\" ~ (mu * g ~ cm ^ {-2})",
      Cw = "\"Water\" ~ (g ~ cm ^ {-2})",
      Cm = "\"LMA\" ~ (g ~ cm ^ {-2})"
    )
  ) %>%
  filter(n() > 5, !is.na(gf_lt), abs(pct_error) < 400) %>%
  ggplot() +
  #aes(x = pct_error, y = short_name, fill = gf_lt) +
  #geom_density_ridges(alpha = 0.5) +
  #geom_point(size = 0.3) +
  #geom_vline(xintercept = 0, linetype = "dashed") +
  aes(x = short_name, y = pct_error, fill = gf_lt, color = gf_lt) +
  geom_violin() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ specparam, scales = "free", labeller = label_parsed) +
  #labs(x = "Percent bias in inversion estimate", y = "Project", fill = "Functional type") +
  labs(y = "Percent bias in inversion estimate", x = "Project", fill = "Functional type") +
  gflt_fill +
  gflt_color +
  guides(color = FALSE) +
  theme(
    legend.position = "bottom",
    #axis.title.y = element_blank()
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90)
  )
ggsave("manuscript/figures/error_by_gf.pdf", proj_pft_plot, width = 8, height = 8)

r2_gf <- error_model %>%
  filter(r2 < 1, N > 5) %>%
  mutate(
    x = interaction(specparam, species_code, short_name) %>%
      fct_reorder(r2, .desc = TRUE)
  ) %>%
  left_join(species_info) %>%
  arrange(x) %>%
  filter(!is.na(gf_lt), N > 5) %>%
  ggplot() +
  aes(x = x, y = r2, fill = gf_lt) +
  #geom_point() +
  geom_col() +
  #geom_segment(aes(x = x, xend = x, y = 0, yend = r2), size = 1.4) +
  #geom_point() +
  facet_wrap(~ specparam, scales = "free", labeller = label_parsed) +
  labs(x = "Species", y = "R2", fill = "Functional type", color = "Functional type") +
  #scale_fill_manual(values = project_colors)
  gflt_color +
  gflt_fill +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom"
  )
ggsave("manuscript/figures/r2_by_gf.pdf", r2_gf, width = 8, height = 8)

############################################################
message("Old validation")

specparam <- "Cab"
trueparam <- "leaf_chltot_per_area"
coords_list <- list()

chl_dat <- valid_data(specparam, trueparam, dat)

valid_subset <- function(specparam, trueparam) {
  trueparam_q <- rlang::sym(trueparam)
  dat %>%
    filter(parameter == specparam, !is.na(!!trueparam_q)) %>%
    rename(lo = `2.5%`, hi = `97.5%`) %>%
    mutate(error = abs(Mean - !!trueparam_q))
}

validate <- function(specparam, trueparam, coords_list = list()) {
  dat_sub <- valid_subset(specparam, trueparam)
  plt <- ggplot(dat_sub) +
    aes_string(x = "Mean", xmin = "lo", xmax = "hi",
               y = trueparam, color = "short_name") +
    geom_point(size = 0.5) +
    geom_errorbarh(color = "grey50", size = 0.5, alpha = 0.5) +
    geom_smooth(method = MASS::rlm, se = FALSE) +
    geom_smooth(mapping = aes(group = 1), method = MASS::rlm, se = FALSE,
                linetype = "solid", color = "black") +
    geom_abline(linetype = "dashed") +
    facet_wrap(~prospect_version, scales = "free") +
    do.call(coord_cartesian, coords_list) +
    theme_bw() +
    scale_color_manual(values = project_colors)

  ggsave(infile(figdir, paste0("validation_", specparam, ".pdf")), plt,
         width = 7, height = 7)

  lm_form <- paste(trueparam, "~", "Mean")
  lm_byproject <- dat_sub %>%
    group_by(short_name, prospect_version) %>%
    nest() %>%
    mutate(
      lmfit = map(data, ~MASS::rlm(formula(lm_form), data = .)),
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
      lmfit = map(data, ~MASS::rlm(formula(lm_form), data = .)),
      coefs = map(lmfit, coef),
      slope = map_dbl(coefs, "Mean"),
      intercept = map_dbl(coefs, "(Intercept)"),
      r2 = map2_dbl(lmfit, data, modelr::rsquare),
      mae = map(data, "error") %>% map_dbl(mean, na.rm = TRUE)
    ) %>%
    filter(r2 < 1) %>%
    select(short_name, species_code, prospect_version, slope, intercept, r2, mae, N) %>%
    mutate(
      sp_proj = interaction(species_code, short_name, prospect_version) %>%
        fct_reorder(r2, .desc = TRUE)
    )
  ggplot(err_byspecies) +
    aes(x = sp_proj, y = r2, fill = short_name) +
    geom_col(position = "identity") +
    facet_wrap(~prospect_version, scales = "free_x") +
    xlab("Species x Project") +
    ylab(expression(R^2)) +
    scale_fill_manual(values = project_colors) +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) -> plt

  ggsave(infile(figdir, paste0("r2_speciesbyproj_", specparam, ".pdf")), plt,
         width = 7, height = 7)

  lm_bsi <- err_byspecies %>%
    left_join(species_info) %>%
    filter(!is.na(leaf_type), growth_form != "vine") %>%
    mutate(growth_form = fct_collapse(growth_form, woody = c("tree", "shrub")))

  lt_colors <- c(broad = "green4", needle = "yellow2")

  plt <- plt %+% lm_bsi +
    aes(fill = leaf_type) +
    scale_fill_manual(values = lt_colors)
  ggsave(infile(figdir, paste0("r2_speciesbyleaf_", specparam, ".pdf")), plt,
         width = 7, height = 7)

  plt <- plt +
    aes(fill = growth_form) +
    labs(fill = "Growth form") +
    scale_fill_manual(values = c(woody = "green4", herb = "purple", graminoid = "orange"))
  ggsave(infile(figdir, paste0("r2_speciesbygf_", specparam, ".pdf")), plt,
         width = 7, height = 7)

  plt <- plt +
    aes(fill = gf_lt) +
    labs(fill = "Growth form") +
    scale_fill_manual(values = c(broadleaf = "green4", conifer = "blue4", herb = "purple", grass = "orange"))
  ggsave(infile(figdir, paste0("r2_speciesbygflt_", specparam, ".pdf")), plt,
         width = 7, height = 7)
}

validate("Cab", "leaf_chltot_per_area")
validate("Car", "leaf_cartot_per_area")
validate("Cw", "leaf_water_thickness")
validate("Cm", "leaf_mass_per_area")
