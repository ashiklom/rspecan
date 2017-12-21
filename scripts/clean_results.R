library(rspecan)
library(units)

results_file <- "processed_results/all_summaries.rds"
raw_results <- readRDS(results_file)

spectra_file <- "processed_data/spectra.h5"

results_wide <- raw_results %>%
  select(
    data_name = dataname,
    spectra_id = data_id,
    parameter,
    mid = Mean,
    lo = `2.5%`,
    hi = `97.5%`
  ) %>%
  gather("stat", "value", mid:hi) %>%
  unite("param_stat", parameter, stat) %>%
  spread(param_stat, value)

read_meta <- function(fname) {
  print(fname)
  draw_all <- readRDS(file.path(meta_dir, fname))
  spec <- draw_all$spectra
  wl <- getwl(spec)
  spspec <- spectra(spec, wl)
  write_spectra(spspec, draw_all$data_name, spectra_file, overwrite = TRUE)
  draw <- draw_all$metadata
  if ("variety" %in% colnames(draw)) {
    draw$variety <- as.character(draw$variety)
  }
  if ("collection_date" %in% colnames(draw)) {
    draw$collection_date <- as.POSIXct(draw$collection_date)
  }
  draw
}

NA_unit <- function(x) {
  out <- NA_real_
  if (inherits(x, "units")) {
    units(out) <- units(x)
  }
  out
}

combine_rows <- function(d1, d2) {
  c1 <- colnames(d1)
  c2 <- colnames(d2)
  both_cols <- intersect(c1, c2)
  d1s <- d1[both_cols]
  d2s <- d2[both_cols]
  dint_list <- Map(c, d1s, d2s)
  dint <- as_tibble(dint_list)
  d1_col <- setdiff(c1, c2)
  d2_col <- setdiff(c2, c1)
  nd1 <- nrow(d1)
  nd2 <- nrow(d2)
  sd1 <- seq_len(nd1)
  sd2 <- nd1 + seq_len(nd2)
  if (length(d1_col) > 0) {
    for (s in d1_col) {
      nau <- NA_unit(d1[[s]])
      dint[[s]] <- c(d1[[s]], rep(nau, nd2))
    }
  }
  if (length(d2_col) > 0) {
    for (s in d2_col) {
      nau <- NA_unit(d2[[s]])
      dint[[s]] <- c(rep(nau, nd1), d2[[s]])
    }
  }
  dint
}

meta_dir <- "processed_data"
meta_files <- dir(meta_dir, pattern = ".*.rds")
meta_list <- map(meta_files, read_meta)
meta_raw <- Reduce(combine_rows, meta_list)

full_raw <- full_join(results_wide, meta_raw)

fill_trait <- function(x, y, mult) {
  inds <- is.na(x) & !is.na(y)
  x[inds] <- units::set_units(y[inds], units::unitless) * mult[inds]
  x
}

naarea <- set_units(NA_real_, "g cm-2")

full_proc <- full_raw %>%
  mutate(
    project = case_when(
      data_name != "curated_leafspec" ~ data_name,
      data_name == "curated_leafspec" ~ projectcode
    )
  ) %>%
  # Assign PROSPECT parameter units
  mutate_at(
    vars(matches("^N_(lo|mid|hi)")),
    ~units::set_units(., units::unitless)
  ) %>%
  mutate_at(
    vars(matches("^Cab_")),
    ~units::set_units(., "ug cm-2")
  ) %>%
  mutate_at(
    vars(matches("^Car_")),
    ~units::set_units(., "ug cm-2")
  ) %>%
  mutate_at(
    vars(matches("^Canth_")),
    ~units::set_units(., "ug cm-2")
  ) %>%
  mutate_at(
    vars(matches("^Cw_")),
    ~units::set_units(., "g cm-2")
  ) %>%
  mutate_at(
    vars(matches("^Cm_")),
    ~units::set_units(., "g cm-2")
  ) %>%
  # Fill mass and area units
  mutate(
    leaf_N_per_area = fill_trait(leaf_N_per_area, leaf_N_pct_mass, leaf_mass_per_area),
    leaf_C_per_area = fill_trait(leaf_C_per_area, leaf_C_pct_mass, leaf_mass_per_area),
    leaf_water_thickness = fill_trait(leaf_water_thickness, leaf_water_pct_mass, leaf_mass_per_area),
    leaf_fiber_per_area = fill_trait(naarea, leaf_fiber_pct_mass, leaf_mass_per_area),
    leaf_lignin_per_area = fill_trait(naarea, leaf_lignin_pct_mass, leaf_mass_per_area),
    leaf_cellulose_per_area = fill_trait(naarea, leaf_cellulose_pct_mass, leaf_mass_per_area),
    leaf_protein_per_area = fill_trait(naarea, leaf_protein_pct_mass, leaf_mass_per_area),
    leaf_betacarotene_per_area = fill_trait(naarea, leaf_betacarotene_pct_mass, leaf_mass_per_area),
    leaf_H_per_area = fill_trait(naarea, leaf_H_pct_mass, leaf_mass_per_area),
    leaf_lutein_per_area = fill_trait(naarea, leaf_lutein_pct_mass, leaf_mass_per_area),
    leaf_neoxanthin_per_area = fill_trait(naarea, leaf_neoxanthin_pct_mass, leaf_mass_per_area),
    leaf_nonpolar_per_area = fill_trait(naarea, leaf_nonpolar_pct_mass, leaf_mass_per_area),
    leaf_O_per_area = fill_trait(naarea, leaf_O_pct_mass, leaf_mass_per_area),
    leaf_polar_per_area = fill_trait(naarea, leaf_polar_pct_mass, leaf_mass_per_area),
    leaf_starch_per_area = fill_trait(naarea, leaf_starch_pct_mass, leaf_mass_per_area),
    leaf_ash_per_area = fill_trait(naarea, leaf_ash_pct_mass, leaf_mass_per_area),
    leaf_fat_per_area = fill_trait(naarea, leaf_fat_pct_mass, leaf_mass_per_area),
    leaf_NSC_per_area = fill_trait(naarea, leaf_NSC_pct_mass, leaf_mass_per_area),
    leaf_K_per_area = fill_trait(naarea, leaf_K_pct_mass, leaf_mass_per_area)
  ) %>%
  # Convert some traits to PROSPECT units for easier comparison
  mutate_at(
    c("leaf_chltot_per_area", "leaf_chla_per_area", "leaf_chlb_per_area",
      "leaf_cartot_per_area"),
    ~units::set_units(., "ug cm-2")
  ) %>%
  mutate_at(
    vars(matches("^Cm_"), matches("^Cw_"), "leaf_mass_per_area", "leaf_water_thickness"),
    ~units::set_units(., "g m-2")
  )

if (FALSE) {
  hist_dat <- full_proc %>%
    select(starts_with("leaf_"), -leaf_age) %>%
    select(sort(colnames(.))) %>%
    filter(rowMeans(is.na(.)) < 1)

  par(mfrow = c(5, 10), mar = c(4, 2, 3, 1))
  for (i in seq_along(hist_dat)) {
    trait <- colnames(hist_dat)[[i]]
    vals <- hist_dat[[i]]
    if (all(is.na(vals))) next
    hist(vals, main = trait)
  }
}

saveRDS(full_proc, "processed_results/complete_results.rds")
