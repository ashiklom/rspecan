library(tidyverse)
library(rspecan)
library(corrplot)
import::from(metar, read_csvy)
import::from(cowplot, plot_grid)

results <- read_csv("spectra_db/cleaned_results.csv")

results_wide <- results %>%
  filter(
    prospect_version == "D",
    parameter %in% variable_df$code
  ) %>%
  mutate(parameter = factor(parameter, variable_df$code)) %>%
  select(project_code, observation_id, parameter, Mean) %>%
  spread(parameter, Mean)

pca_dat <- results_wide %>%
  select(variable_df$code) %>%
  setNames(variable_df$shortname)

pcfit <- princomp(pca_dat, cor = TRUE)
pcvec <- pcfit$loadings[, ] %>%
  as_tibble() %>%
  mutate(variable = variable_df$shortname) %>%
  select(variable, everything())
pcpoints <- pca_dat %>%
  add_column(!!!as_tibble(pcfit$scores))

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

pdf(infile("manuscript/figures/prospect_pca.pdf"), height = 3.5, width = 7)
plot_grid(phist, p12, p23, nrow = 1)
dev.off()

metadata <- read_csvy("spectra_db/cleaned_metadata.csvy") %>%
  mutate_if(is.character, na_if, "")

species_info_raw <- read_csvy("spectra_db/species_info.csvy") %>%
  mutate_if(is.character, na_if, "")

clim_dat <- readRDS("extdata/worldclim_met.rds") %>%
  select(-site_name) %>%
  unnest(met_data) %>%
  select(-ends_with("1")) %>%
  mutate_if(is_double, ~if_else(. < -1e10, NA_real_, .))

species_clim <- metadata %>%
  distinct(species_code, latitude, longitude) %>%
  left_join(clim_dat) %>%
  group_by(species_code) %>%
  summarize_at(c("AMT", "AP"), mean, na.rm = TRUE) %>%
  mutate(
    biome = case_when(
      AMT < 0 ~ "arctic",
      AMT > 20 ~ "tropical",
      TRUE ~ "temperate"
    )
  )

species_info <- species_info_raw %>%
  distinct(species_code, .keep_all = TRUE) %>%
  inner_join(species_clim)

dat <- left_join(results_wide, metadata) %>%
  select(
    project_code, observation_id, species_code, latitude, longitude,
    !!!variable_df$code, starts_with("leaf"),
    -leaf_temperature, -leaf_age,
    -leaf_prospect_N, -leaf_area, -leaf_thickness
  )

dat_sub <- dat %>%
  select(
    project_code:longitude,
    !!!variable_df$code,
    matches("^leaf_(N|C|lignin|cellulose)_per_area$"),
    matches("^leaf_(Vcmax|Jmax)_area")
  )

corr_by_species <- function(xvar, yvar, dat) {
  xq <- rlang::sym(as.character(xvar))
  yq <- rlang::sym(as.character(yvar))
  dat %>%
    filter(!is.na(!!xq), !is.na(!!yq)) %>%
    group_by(species_code) %>%
    filter(n() >= 10) %>%
    summarize(
      corr = cor(!!xq, !!yq),
      N = n()
    )
}

corr_grid <- expand.grid(
  prospect = variable_df$code,
  trait = dat_sub %>% select(starts_with("leaf_")) %>% colnames()
) %>%
  as_tibble() %>%
  mutate(data = map2(prospect, trait, corr_by_species, dat = dat_sub)) %>%
  unnest() %>%
  left_join(species_info)

sub_trait <- function(x) {
  str_match(x, "^leaf_([[:alpha:]]+)(?:_per)?_area")[,2]
}

corr_grid %>%
  mutate(
    species_code = fct_reorder(species_code, corr, mean),
    prospect = lvls_revalue(prospect, variable_df$shortname),
    trait = fct_relabel(trait, sub_trait)
  ) %>%
  ggplot() +
  aes(x = species_code, y = corr, alpha = N) +
  geom_col() +
  #geom_hline(yintercept = 0, lty = "dashed") +
  #geom_hline(yintercept = c(0.5, -0.5), lty = "dotted") +
  facet_grid(trait ~ prospect, scales = "free_y", space = "free_y") +
  ylim(-1, 1) +
  coord_flip() +
  scale_alpha_continuous(trans = "log10") +
  labs(
    y = "Correlation coefficient",
    alpha = "Sample size"
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_blank(),
    axis.text = element_text(size = rel(0.4)),
    panel.spacing = unit(0.2, "lines")
  ) -> plt
ggsave(infile("manuscript", "figures", "trait_correlations_areabars.pdf"), plt,
       width = 7, height = 11, units = "in")

order_vals <- . %>%
  select(
    !!!df2dict(variable_df, "code", "shortname"),
    ends_with("per_area"),
    ends_with("_area"),
    leaf_water_thickness,
    ends_with("pct_mass"),
    ends_with("_mass"),
    everything()
  )

dat_vals <- dat %>% select(-project_code, -observation_id, -species_code) %>%
  order_vals
all_corr <- cor(dat_vals, use = "pairwise.complete.obs")[, variable_df$shortname]

pdf(infile("manuscript/figures/trait_correlations_all.pdf"))
corrplot(all_corr)
dev.off()

species_means <- dat %>%
  select(-project_code, -observation_id) %>%
  group_by(species_code) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  select_if(~sum(!is.na(.)) > 3)

species_vals <- species_means %>%
  select(-species_code) %>%
  order_vals
species_corr <- cor(species_vals, use = "pairwise.complete.obs")[, variable_df$shortname]
pdf(infile("manuscript/figures/trait_correlations_species.pdf"))
corrplot(species_corr)
dev.off()
