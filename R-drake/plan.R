library(drake)

pkgconfig::set_config("drake::strings_in_dots" = "literals")

analysis <- drake_plan(
  # Prepare results
  barnes_results_file = file_in("spectra_db/barnes_results.csv"),
  all_results_file = file_in("spectra_db/all_results.csv"),
  raw_results = get_raw_results(all_results_file, barnes_results_file),
  all_metadata = rspecan::get_metadata(
    "spectra_db",
    metadata_cols = c("project_code", "short_name", "long_name")
  ),
  drop_metadata = all_metadata %>%
    dplyr::filter(
      target_type %in% c("rock", "soil") | fresh_dry == "dry"
    ),
  results = get_good_results(raw_results, drop_metadata),
  bad_results = dplyr::anti_join(raw_results, results),
  wide_results = make_wide_results(results, "D"),
  metadata = get_good_metadata(all_metadata, results),
  bad_metadata = dplyr::anti_join(all_metadata, results),
  biome_labels = readr::read_csv2(file_in("data/biome_labels.csv")),
  biome_polygons = readr::read_csv2(file_in("data/biome_polygons.csv")),
  climate_data = get_climate_data(file_in("extdata/worldclim_met.rds")),
  sites_all = get_site_data(metadata, climate_data),
  project_colors = readr::read_csv(file_in("spectra_db/project_colors.csv")) %>%
    dplyr::semi_join(sites_all),
  sites = dplyr::semi_join(sites_all, project_colors),
  site_climate_gg = site_climate_plot(sites, biome_polygons, biome_labels, project_colors),
  site_map_gg = site_map_plot(sites, project_colors),
  species_info = get_species_info(file_in("spectra_db/species_info.csvy"), metadata),
  species_info_gf = assign_species_gf(species_info),
  results_d_long = combine_results_long(results, metadata, species_info),
  anova_all_dat = anova_all(results_d_long),
  anova_all_gg = anova_all_plot(anova_all_dat),
  anova_interspecific_dat = anova_interspecific(results_d_long, mod_params),
  anova_interspecific_gg = anova_interspecific_plot(anova_interspecific_dat, mod_params),
  phenology_dat = setup_phenology_dat(results, metadata),
  phenology_gg = phenology_plot(phenology_dat),
  project_table_dat = setup_project_table(metadata),
  prospect_pairs_plotnames = all_result_pairs(results),
  wide_results_sorted = wide_results_sort(wide_results),
  pca_plot_gg = pca_plot(wide_results_sorted),
  correlation_dat = setup_correlation_data(wide_results_sorted, metadata),
  correlation_plot_dat = setup_correlation_plot(correlation_dat, species_info_gf),
  correlation_plot_gg = correlation_plot(correlation_plot_dat),
  species_mean_correlations = setup_correlation_species_means(correlation_dat),
  intraspecific_correlation_ridge_gg = intraspecific_correlation_ridge_plot(correlation_plot_dat)
)

manuscript_figs <- drake_plan(
  site_climate_man = ggsave(
    file_out("manuscript/figures/data_climate.pdf"),
    site_climate_gg,
    width = 10,
    height = 7,
    units = "in"
  ),
  site_map_man = ggsave(
    file_out("manuscript/figures/data_map.pdf"),
    site_map_gg,
    width= 7,
    height = 5
  ),
  anova_all_man = ggsave(
    file_out("manuscript/figures/within_vs_across.pdf"),
    anova_all_gg
  ),
  anova_interspecific_man = ggsave(
    file_out("manuscript/figures/across_species_anova.pdf"),
    anova_interspecific_gg
  ),
  phenology_man = ggsave(
    file_out("manuscript/figures/trait_phenology.pdf"),
    phenology_gg
  ),
  project_table_man = write_project_table(
    project_table_dat,
    file_out("manuscript/figures/project_table.tex")
  ),
  pca_plot_man = cowplot::ggsave(
    file_out("manuscript/figures/prospect_pca.pdf"),
    pca_plot_gg,
    width = 7, height = 3.5
  ),
  correlation_plot_man = cowplot::save_plot(
    file_out("manuscript/figures/trait_correlations_lollipop.pdf"),
    correlation_plot_gg,
    base_width = 8,
    base_height = 9
  ),
  correlation_species_means_plot_man = correlation_species_means_plot(
    species_mean_correlations,
    file_out("manuscript/figures/trait_correlations_species.pdf")
  )
)

presentation_figs <- drake_plan(
  correlation_species_means_present = {
    pdf(file_out("manuscript/figures/zpres_trait_correlations_species.pdf"))
    ii <- seq_len(nrow(rspecan::variable_df))
    corrplot::corrplot(
      species_mean_correlations[-ii, ii]
    )
    dev.off()
  },
  intraspecific_correlation_ridge_present = cowplot::save_plot(
    file_out("manuscript/figures/zpres_correlation_ridge.pdf"),
    intraspecific_correlation_ridge_gg,
    base_height = 6,
    base_width = 9
  )
)

my_plan <- rbind(analysis, manuscript_figs, presentation_figs)
