library(ggplot2)
import::from("magrittr", "%>%")

# Load basic metadata, and remove non-vegetation (rocks and soil) and
# dry leaves.
basic_metadata <- function() {
  rspecan::get_metadata(
    "spectra_db",
    metadata_cols = c("project_code", "short_name", "long_name")
  ) %>%
    dplyr::filter(
      target_type %in% c("rock", "soil") | fresh_dry == "dry"
    )
}

get_raw_results <- function(all_file, barnes_file) {
  barnes_results <- readr::read_csv(barnes_file)
  raw_results <- readr::read_csv("spectra_db/all_results.csv") %>%
    dplyr::select(-specdb) %>%
    dplyr::filter(project_code != "barnes_2017") %>%
    dplyr::bind_rows(barnes_results)
  raw_results
}

get_good_results <- function(raw_results, drop_metadata) {
  drop_results <- raw_results %>%
    dplyr::select(project_code, observation_id, prospect_version, parameter, Mean) %>%
    tidyr::spread(parameter, Mean) %>%
    dplyr::filter_at(
      c("N", "Cab", "Car", "Canth", "Cbrown", "Cw", "Cm"),
      dplyr::any_vars(. > quantile(., 0.95, na.rm = TRUE))
    ) %>%
    dplyr::distinct(project_code, observation_id, prospect_version)
  raw_results %>%
    dplyr::anti_join(drop_results) %>%
    dplyr::anti_join(drop_metadata)
}

make_wide_results <- function(results, prospect_version = "D") {
  results %>%
    dplyr::filter(prospect_version == !!prospect_version) %>%
    dplyr::select(-specdb) %>%
    tidyr::gather(stat, value, Mean:`97.5%`) %>%
    dplyr::filter(stat %in% c("Mean", "2.5%", "97.5%")) %>%
    dplyr::mutate(
      stat = dplyr::recode(stat, "2.5%" = "lo", "97.5%" = "hi")
    ) %>%
    tidyr::unite(param_stat, parameter, stat) %>%
    tidyr::spread(param_stat, value) %>%
    dplyr::rename_at(dplyr::vars(ends_with("_Mean")), ~stringr::str_remove(., "_Mean"))
}

# Fill in more trait measurements
fill_trait <- function(x, y, mult) {
  dplyr::case_when(
    !is.na(x) ~ x,
    !is.na(y) & !is.na(mult) ~ y * mult,
    TRUE ~ NA_real_
  )
}

get_good_metadata <- function(all_metadata, results) {
  md <- dplyr::semi_join(all_metadata, results)
  cmm <- 1e4  # Convert g cm-2 to g m-2
  md <- md %>%
    dplyr::mutate(
      leaf_anth_per_area = dplyr::if_else(leaf_anth_per_area < 0, NA_real_, leaf_anth_per_area),
      leaf_N_per_area = fill_trait(leaf_N_per_area, leaf_N_pct_mass, leaf_mass_per_area * cmm),
      leaf_C_per_area = fill_trait(leaf_C_per_area, leaf_C_pct_mass, leaf_mass_per_area * cmm),
      leaf_chla_per_area = fill_trait(leaf_chla_per_area, leaf_chla_pct_mass, leaf_mass_per_area * 1e6),
      leaf_chlb_per_area = fill_trait(leaf_chlb_per_area, leaf_chlb_pct_mass, leaf_mass_per_area * 1e6),
      leaf_chltot_per_area = dplyr::if_else(is.na(leaf_chltot_per_area), leaf_chla_per_area + leaf_chlb_per_area, leaf_chltot_per_area),
      leaf_nonpolar_per_area = leaf_nonpolar_pct_mass * leaf_mass_per_area * cmm,
      leaf_polar_per_area = leaf_polar_pct_mass * leaf_mass_per_area * cmm,
      leaf_cellulose_per_area = leaf_cellulose_pct_mass * leaf_mass_per_area * cmm,
      leaf_lignin_per_area = leaf_lignin_pct_mass * leaf_mass_per_area * cmm,
      leaf_H_per_area = leaf_H_pct_mass * leaf_mass_per_area * cmm,
      leaf_lutein_per_area = leaf_lutein_pct_mass * leaf_mass_per_area * cmm,
      leaf_neoxanthin_per_area = leaf_neoxanthin_pct_mass * leaf_mass_per_area * cmm,
      leaf_betacarotene_per_area = leaf_betacarotene_pct_mass * leaf_mass_per_area * cmm,
      leaf_fiber_per_area = leaf_fiber_pct_mass * leaf_mass_per_area * cmm,
      leaf_ash_per_area = leaf_ash_pct_mass * leaf_mass_per_area * cmm,
      leaf_fat_per_area = leaf_fat_pct_mass * leaf_mass_per_area * cmm,
      leaf_NSC_per_area = leaf_NSC_pct_mass * leaf_mass_per_area * cmm,
      leaf_protein_per_area = leaf_protein_pct_mass * leaf_mass_per_area * cmm,
      leaf_K_per_area = leaf_K_pct_mass * leaf_mass_per_area * cmm,
      leaf_O_per_area = leaf_O_pct_mass * leaf_mass_per_area * cmm,
      leaf_starch_per_area = leaf_starch_pct_mass * leaf_mass_per_area * cmm
    ) %>%
    dplyr::select_if(~!all(is.na(.)))
}

get_climate_data <- function(climate_data_file) {
  readRDS(climate_data_file) %>%
    tidyr::unnest(met_data) %>%
    dplyr::select(-latitude1, -longitude1, -site_name) %>%
    dplyr::filter(AMT > -1e5) %>%
    dplyr::mutate(AP = AP / 10)
}

get_site_data <- function(metadata, climate_data) {
  metadata %>%
    dplyr::distinct(project_code, short_name, latitude, longitude) %>%
    dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
    dplyr::left_join(climate_data %>% dplyr::select(latitude, longitude, AMT, AP))
}

site_climate_plot <- function(sites, biome_polygons, biome_labels, project_colors) {
  ggplot() +
    geom_polygon(
      data = biome_polygons,
      mapping = aes(x = temp_degC, y = prec_mm, fill = biome)
    ) +
    geom_label(
      data = biome_labels,
      mapping = aes(x = y, y = x, label = biome_label)
    ) +
    geom_point(
      data = sites,
      mapping = aes(x = AMT, y = AP, color = short_name),
      size = 3
    ) +
    scale_fill_manual(values = rspecan::df2dict(biome_labels, "color", "biome") %>% as.character()) +
    scale_color_manual(values = rspecan::df2dict(project_colors, "color", "short_name")) +
    xlab(expression("Annual mean temperature" ~ (degree * C))) +
    ylab("Annual precipitation (cm)") +
    guides(fill = FALSE) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

site_map_plot <- function(sites, project_colors) {
  proj_colors <- rspecan::df2dict(project_colors, "color", "short_name")
  
  robinson <- sf::st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  gall_stereo <- sf::st_crs("+proj=gall +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  my_proj <- gall_stereo
  wgs <- sf::st_crs(4326)
  dat_geog <- sites %>%
    dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
    dplyr::mutate(geometry = purrr::map2(longitude, latitude, ~sf::st_point(c(.x, .y)))) %>%
    sf::st_sf(crs = wgs) %>%
    sf::st_transform(my_proj) %>%
    dplyr::group_by(project_code) %>%
    dplyr::mutate(rn = dplyr::row_number(), N = n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(rn), dplyr::desc(N))
  mapdat_sf <- rnaturalearth::ne_countries(returnclass = "sf") %>%
    sf::st_transform(my_proj) %>%
    tibble::as_tibble() %>%
    sf::st_as_sf()

  y_lim <- c(-15, 70)
  x_lim <- c(-160, 50)
  xy_lim <- cbind(x_lim, y_lim) %>%
    sf::st_multipoint() %>%
    sf::st_sfc(crs = wgs) %>%
    sf::st_transform(my_proj) %>%
    sf::st_bbox()

  ggplot(dat_geog) +
    geom_sf(data = mapdat_sf, inherit.aes = FALSE, fill = "grey50", color = NA) +
    geom_sf(aes(fill = short_name), color = "black", size = 3, pch = 21) +
    coord_sf(xlim = xy_lim[c("xmin", "xmax")], ylim = xy_lim[c("ymin", "ymax")]) +
    scale_fill_manual(values = proj_colors) +
    guides(fill = guide_legend(title = element_blank(), override.aes = list(color = NA))) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.line = element_blank(),
      panel.grid = element_line(color = NA)
    )
}

get_species_info <- function(species_file, metadata) {
  meta_sub <- metadata %>% dplyr::select(species_code)
  readr::read_csv(
    species_file,
    col_types = readr::cols(.default = "c", nitrogen_fixer = "l",
                            shade_tolerance_numeric = "c", try_species_ID = "i",
                            myco_is_am = "l", is_shade_intolerant = "l")
  ) %>%
    dplyr::semi_join(meta_sub) %>%
    dplyr::select(-Database_ID, -scientific_name, -variety, -genus, -species)
}

combine_results_long <- function(results, metadata, species_info, prospect_version = "D") {
  results %>%
    dplyr::filter(
      prospect_version == !!prospect_version,
      !parameter %in% c("Cbrown", "residual")
    ) %>%
    dplyr::left_join(metadata) %>%
    dplyr::left_join(species_info) %>%
    dplyr::filter(!is.na(species_code))
}

anova_all <- function(results_long) {
  all_fit <- results_long %>%
    dplyr::group_by(parameter) %>%
    tidyr::nest() %>%
    dplyr::mutate(lmfit = purrr::map(data, lm, formula = Mean ~ species_code))
  all_anova <- all_fit %>%
    dplyr::mutate(
      lm_anova = purrr::map(lmfit, car::Anova),
      tidy_anova = purrr::map(lm_anova, broom::tidy)
    ) %>%
    tidyr::unnest(tidy_anova)
  anova_plt <- all_anova %>%
    dplyr::group_by(parameter) %>%
    dplyr::mutate(fraq_sq = sumsq / sum(sumsq)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      parameter = factor(parameter, rspecan::variable_df$code) %>%
        forcats::lvls_revalue(rspecan::variable_df$shortname),
      term = factor(term, rev(unique(term))) %>%
        forcats::lvls_revalue(c("within-species", "across-species"))
    )
  anova_plt
}

anova_all_plot <- function(anova_all) {
  ggplot(anova_all) +
    aes(x = parameter, y = fraq_sq, fill = term) +
    geom_col() +
    scale_fill_manual(values = c("skyblue", "green4")) +
    labs(
      x = "Optical trait",
      y = "Fraction of variability explained",
      fill = "Variability"
    ) +
    theme_bw()
}

mod_params <- c(
  green4 = "phenology",
  yellow3 = "leaf_type",
  purple = "ps_type",
  brown = "growth_form",
  pink = "nitrogen_fixer",
  orange = "myco_is_am"
)

anova_interspecific <- function(results_long, mod_params) {
  mod_colors <- c(rspecan::swap_names(mod_params), Residuals = "grey")

  lm_form <- paste0("value ~ ", paste(mod_params, collapse = " + "))

  species_means <- results_long %>%
    dplyr::filter_at(mod_params, dplyr::all_vars(!is.na(.))) %>%
    dplyr::group_by(parameter, species_code, !!!rlang::syms(unname(mod_params))) %>%
    dplyr::summarize(value = mean(Mean, na.rm = TRUE)) %>%
    dplyr::ungroup()

  species_fit <- species_means %>%
    dplyr::group_by(parameter) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      lmfit = purrr::map(data, lm, formula = formula(lm_form)),
      lm_anova = purrr::map(lmfit, car::Anova),
      tidy_fit = purrr::map(lmfit, broom::tidy),
      tidy_anova = purrr::map(lm_anova, broom::tidy)
    )

  species_plot <- species_fit %>%
    tidyr::unnest(tidy_anova) %>%
    dplyr::group_by(parameter) %>%
    dplyr::mutate(fraq_sq = sumsq / sum(sumsq)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      parameter = factor(parameter, rspecan::variable_df$code) %>%
        forcats::lvls_revalue(rspecan::variable_df$shortname),
      term = factor(term, rev(unique(term)))
    )

  splot2 <- species_plot %>%
    dplyr::select(parameter, term, p.value, fraq_sq) %>%
    dplyr::mutate(is_signif = !is.na(p.value) & p.value < 0.1) %>%
    dplyr::group_by(parameter) %>%
    dplyr::mutate(term2 = forcats::fct_rev(term)) %>%
    dplyr::arrange(parameter, term2) %>%
    dplyr::mutate(
      hi = cumsum(fraq_sq),
      lo = hi - fraq_sq,
      pos = dplyr::if_else(is_signif, (lo + hi) / 2, NA_real_),
      ) %>%
    dplyr::select(-lo, -hi, -is_signif)

  splot2
}

anova_interspecific_plot <- function(anova_interspecific_dat, mod_params) {
  mod_colors <- c(rspecan::swap_names(mod_params), Residuals = "grey")
  ggplot(anova_interspecific_dat) +
    aes(x = parameter, y = fraq_sq, fill = term) +
    geom_col() +
    geom_point(aes(y = pos), shape = 8, show.legend = FALSE) +
    scale_fill_manual(values = mod_colors) +
    labs(
      x = "Optical trait",
      y = "Fraction of variance explained",
      fill = "Species attribute"
    ) +
    theme_bw()
}

setup_phenology_dat <- function(results, metadata) {
  metadata %>%
    dplyr::filter(
      project_code == "yang_pheno",
      species_code == "QURU"
    ) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "") %>%
    dplyr::select_if(~!all(is.na(.))) %>%
    dplyr::left_join(results) %>%
    dplyr::mutate(
      collection_date = lubridate::ymd(collectiondate),
      year = lubridate::year(collection_date),
      doy = lubridate::yday(collection_date),
      parameter = factor(parameter, rspecan::variable_df$code)
    ) %>%
    dplyr::filter(
      year == 2011,
      prospect_version == "D",
      !is.na(parameter),
      !(parameter == "Canth" & Mean > 10)
    ) %>%
    dplyr::mutate(parameter = `levels<-`(parameter, rspecan::variable_df$short_units))
}

phenology_plot <- function(phenology_dat) {
  ggplot(phenology_dat) +
    aes(x = collection_date, y = Mean, color = sun_shade) +
    geom_point() +
    geom_smooth(method = "loess") +
    facet_wrap(~parameter, scales = "free_y", labeller = label_parsed) +
    theme_bw() +
    ylab("Inversion mean estimate") +
    theme(axis.title.x = element_blank())
}

setup_project_table <- function(metadata) {
  metadata %>%
    dplyr::group_by(short_name, long_name) %>%
    dplyr::summarize(
      Samples = dplyr::n(),
      Species = dplyr::n_distinct(species_code),
      Sites = dplyr::n_distinct(latitude, longitude)
    ) %>%
    dplyr::rename("Short name" = short_name, "Long name" = long_name)
}

write_project_table <- function(project_table_dat, outfile) {
  knitr::kable(project_table_dat, format = "latex", booktabs = TRUE) %>%
    kableExtra::kable_styling(font_size = 8) %>%
    kableExtra::column_spec(2, width = "25em") %>%
    cat(file = outfile)
}
