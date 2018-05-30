assign_species_gf <- function(species_info) {
  species_info %>%
    dplyr::mutate(
      gf_lt = dplyr::case_when(
        leaf_type == "needle" ~ "conifer",
        growth_form %in% c("tree", "shrub") ~ "broadleaf",
        growth_form == "graminoid" ~ "grass",
        growth_form == "herb" ~ "herb",
        TRUE ~ NA_character_
      ),
      gf_lt = factor(gf_lt, c("broadleaf", "conifer", "grass", "herb"))
    )
}

wide_results_sort <- function(wide_results) {
  results_wide <- wide_results %>%
    dplyr::select(project_code, observation_id, !!!rspecan::variable_df$code)
}

pca_plot <- function(results_wide) {
  pca_dat <- results_wide %>%
    dplyr::select(rspecan::variable_df$code) %>%
    setNames(rspecan::variable_df$shortname)
  pcfit <- princomp(pca_dat, cor = TRUE)
  pcvec <- pcfit$loadings[, ] %>%
    tibble::as_tibble() %>%
    dplyr::mutate(variable = rspecan::variable_df$shortname) %>%
    dplyr::select(variable, dplyr::everything())
  pcpoints <- pca_dat %>%
    tibble::add_column(!!!tibble::as_tibble(pcfit$scores))
  pcplot <- function(x, y, z = 10, nudge = 0.5, ...) {
    vx <- pcvec[[x]] * z
    vy <- pcvec[[y]] * z
    lx <- vx + sign(vx) * nudge
    ly <- vy + sign(vy) * nudge
    ll <- pcvec[["variable"]]
    ggplot() +
      geom_point(
        aes_string(x = x, y = y),
        data = pcpoints,
        ...
      ) +
      geom_segment(
        aes(xend = vx, yend = vy),
        x = 0, y = 0, color = "red",
        arrow = arrow(length = unit(0.03, "npc"))
      ) +
      geom_text(aes(x = lx, y = ly, label = ll), fontface = "bold") +
      theme_bw()
  }
  phist <- ggplot() +
    aes(x = as.character(seq_along(pcfit$sdev)),
        y = cumsum(pcfit$sdev ^ 2) / sum(pcfit$sdev ^ 2)) +
    geom_point() +
    geom_line(group = 1) +
    xlab("Component") +
    ylab("Cumulative fraction of variance explained") +
    ylim(0, 1) +
    theme_bw()
  p12 <- pcplot("Comp.1", "Comp.2", color = "grey40", alpha = 0.3)
  p23 <- pcplot("Comp.2", "Comp.3", color = "grey40", alpha = 0.3)
  cowplot::plot_grid(phist, p12, p23, nrow = 1)
}

## Input metadata
setup_correlation_data <- function(results_wide, metadata) {
  dplyr::left_join(results_wide, metadata) %>%
    dplyr::select(
      project_code, observation_id, species_code, latitude, longitude,
      !!!rspecan::variable_df$code, dplyr::starts_with("leaf"),
      -leaf_temperature, -leaf_age,
      -leaf_prospect_N, -leaf_area, -leaf_thickness
    )
}

setup_correlation_plot <- function(correlation_data, species_info_gf) {
  dat_sub <- correlation_data %>%
    dplyr::select(
      project_code:longitude,
      !!!rspecan::variable_df$code,
      dplyr::matches("^leaf_(N|C|lignin|cellulose)_per_area$"),
      dplyr::matches("^leaf_(Vcmax|Jmax)_area")
    )
  dat_traits <- dat_sub %>% dplyr::select(dplyr::starts_with("leaf_")) %>% colnames()
  corr_grid <- expand.grid(
    prospect = rspecan::variable_df$code,
    trait = dat_traits
  ) %>%
    tibble::as_tibble() %>%
  dplyr::mutate(
    data = purrr::map2(prospect, trait, corr_by_species, dat = dat_sub)
  ) %>%
  tidyr::unnest() %>%
  dplyr::left_join(species_info_gf)

  corr_plt <- corr_grid %>%
    dplyr::mutate(
      species_code = forcats::fct_reorder(species_code, corr, mean),
      prospect = forcats::lvls_revalue(prospect, rspecan::variable_df$shortname),
      trait = forcats::fct_relabel(trait, sub_trait)
    )
}

corr_by_species <- function(xvar, yvar, dat) {
  xq <- rlang::sym(as.character(xvar))
  yq <- rlang::sym(as.character(yvar))
  dat %>%
    dplyr::filter(!is.na(!!xq), !is.na(!!yq)) %>%
    dplyr::group_by(species_code) %>%
    dplyr::filter(n() >= 10) %>%
    dplyr::summarize(
      corr = cor(!!xq, !!yq),
      N = dplyr::n()
    )
}

sub_trait <- function(x) {
  stringr::str_match(x, "^leaf_([[:alpha:]]+)(?:_per)?_area")[, 2]
}

plot_corr <- function(dat) {
  dat %>%
    dplyr::mutate(x = as.character(species_code)) %>%
    dplyr::arrange(gf_lt) %>%
    ggplot() +
    aes(x = species_code, xend = species_code, y = corr, color = gf_lt) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_segment(aes(y = 0, yend = corr)) +
    geom_point() +
    facet_grid(trait ~ prospect, scales = "free_y", space = "free_y") +
    coord_flip() +
    scale_alpha_continuous(trans = "log10") +
    scale_color_manual(values = c("broadleaf" = "green4", "conifer" = "blue4", "herb" = "purple", "grass" = "orange")) +
    scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1, 1)) +
    labs(
      y = "Correlation coefficient",
      color = "Functional type"
    ) +
    theme_grey() +
    theme(
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 90, size = rel(0.7)),
      axis.text.y = element_text(size = rel(0.8)),
      strip.text.y = element_text(angle = 0, size = rel(0.75)),
      strip.text.x = element_text(size = rel(0.75)),
      panel.spacing = unit(0.2, "lines"),
      legend.position = "bottom"
    )
}

correlation_plot <- function(corr_plt) {
  cn_plot <- plot_corr(corr_plt %>% dplyr::filter(trait %in% c("N", "C")))
  other_plot <- plot_corr(corr_plt %>% dplyr::filter(!trait %in% c("N", "C")))
  leg <- cowplot::get_legend(cn_plot)
  both_plot <- cowplot::plot_grid(
    cowplot::plot_grid(cn_plot + guides(color = FALSE), other_plot + guides(color = FALSE), nrow = 1),
    leg,
    rel_heights = c(1, 0.03),
    nrow = 2
  )
}

setup_correlation_species_means <- function(correlation_data) {
  species_means <- correlation_data %>%
    dplyr::select(-project_code, -observation_id) %>%
    dplyr::group_by(species_code) %>%
    dplyr::summarize_all(mean, na.rm = TRUE) %>%
    dplyr::select_if(~sum(!is.na(.)) >= 10)
  use_y <- c(
    "N" = "leaf_N_per_area",
    "C" = "leaf_C_per_area",
    "cellulose" = "leaf_cellulose_per_area",
    "lignin" = "leaf_lignin_per_area"
  )
  species_vals <- species_means %>%
    dplyr::select(-species_code) %>%
    dplyr::select(
      !!!rspecan::df2dict(rspecan::variable_df, "code", "shortname"),
      dplyr::ends_with("per_area"),
      dplyr::ends_with("_area"),
      leaf_water_thickness,
      dplyr::ends_with("pct_mass"),
      dplyr::ends_with("_mass"),
      dplyr::everything()
    ) %>%
    dplyr::select(
      1:6, dplyr::matches("_area"),
      -dplyr::matches("chl", ignore.case = FALSE),
      -dplyr::matches("cartot", ignore.case = FALSE),
      -dplyr::matches("fiber"),
      -leaf_H_per_area, -leaf_O_per_area, -leaf_mass_per_area
    ) %>%
    dplyr::rename_all(~stringr::str_remove(., "leaf_")) %>%
    dplyr::rename_all(~stringr::str_remove(., "_per_area"))
  species_corr <- cor(species_vals, use = "pairwise.complete.obs")
  species_corr
}

correlation_species_means_plot <- function(species_mean_correlations, outfile) {
  pdf(outfile)
  corrplot::corrplot.mixed(
    species_mean_correlations,
    upper = "number", lower = "circle",
    number.cex = 0.7, tl.cex = 0.5
  )
  dev.off()
}

intraspecific_correlation_ridge_plot <- function(correlation_plot_dat) {
  ggplot(correlation_plot_dat) +
    aes(x = corr, y = prospect) +
    geom_point(aes(color = gf_lt)) +
    ggridges::geom_density_ridges(aes(fill = gf_lt)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~trait) +
    labs(
      x = "Intra-specific correlation coefficient",
      y = "Optical trait",
      color = "PFT", fill = "PFT"
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = rel(1.5)),
      axis.text.y = element_text(size = rel(1.3)),
      strip.text = element_text(size = rel(1.3))
    )
}
