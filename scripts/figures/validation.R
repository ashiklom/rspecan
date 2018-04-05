library(tidyverse)
library(rspecan)
library(metar)
import::from("knitr", "kable")

figdir <- indir("manuscript", "figures")

results <- read_csv("spectra_db/cleaned_results.csv")
metadata <- get_metadata("spectra_db")
species_info <- read_csvy("spectra_db/species_info.csvy")

project_colors <- read_csv("spectra_db/project_colors.csv") %>%
  df2dict("color", "short_name")

md_sub <- metadata %>%
  select(
    project_code, short_name,
    observation_id,
    species_code,
    leaf_chltot_per_area,
    leaf_cartot_per_area,
    leaf_water_thickness,
    leaf_mass_per_area
  ) %>%
  filter_at(vars(starts_with("leaf")), any_vars(!is.na(.)))

dat <- left_join(results, md_sub) %>%
  filter(!is.na(short_name))

specparam <- "Cab"
trueparam <- "leaf_chltot_per_area"

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
      lmfit = map(data, ~lm(formula(lm_form), data = .)),
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
validate("Car", "leaf_cartot_per_area", list(xlim = c(0, 40)))
validate("Cw", "leaf_water_thickness", list(xlim = c(0, 0.11), ylim = c(0, 0.06)))
validate("Cm", "leaf_mass_per_area", list(xlim = c(0, 0.1), ylim = c(0, 0.04)))
