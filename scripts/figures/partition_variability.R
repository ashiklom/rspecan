library(tidyverse)
library(rspecan)
import::from(metar, read_csvy)

results <- read_csv("spectra_db/cleaned_results.csv")

metadata <- read_csvy("spectra_db/cleaned_metadata.csvy") %>%
  mutate_if(is.character, na_if, "") %>%
  select(-scientific_name, -variety, -genus, -species)

spp_info <- read_csv(
  "spectra_db/species_info.csvy",
  col_types = cols(.default = "c", nitrogen_fixer = "l",
                   shade_tolerance_numeric = "c", try_species_ID = "i",
                   myco_is_am = "l", is_shade_intolerant = "l")
) %>%
  semi_join(metadata)

spp_info %>%
  filter(is.na(growth_form))

raw_data <- results %>%
  filter(
    prospect_version == "D",
    !parameter %in% c("Cbrown", "residual")
  ) %>%
  left_join(metadata) %>%
  left_join(spp_info) %>%
  filter(!is.na(species_code))

############################################################
# Inter vs. intra-specific variability
############################################################
all_fit <- raw_data %>%
  group_by(parameter) %>%
  nest() %>%
  mutate(lmfit = map(data, lm, formula = Mean ~ species_code))
all_anova <- all_fit %>%
  mutate(
    lm_anova = map(lmfit, car::Anova),
    tidy_anova = map(lm_anova, broom::tidy)
  ) %>%
  unnest(tidy_anova)
anova_plt <- all_anova %>%
  group_by(parameter) %>%
  mutate(fraq_sq = sumsq / sum(sumsq)) %>%
  ungroup() %>%
  mutate(
    parameter = factor(parameter, variable_df$code) %>% lvls_revalue(variable_df$shortname),
    term = factor(term, rev(unique(term))) %>%
      lvls_revalue(c("within-species", "across-species"))
  )

plt <- ggplot(anova_plt) +
  aes(x = parameter, y = fraq_sq, fill = term) +
  geom_col() +
  scale_fill_manual(values = c("skyblue", "green4")) +
  labs(
    x = "Optical trait",
    y = "Fraction of variability explained",
    fill = "Variability"
  ) +
  theme_bw()
if (interactive()) print(plt)
ggsave(infile("manuscript/figures/within_vs_across.pdf"), plt)

############################################################
# Partitioning across-species variability
############################################################
mod_params <- c(
  green4 = "phenology",
  yellow3 = "leaf_type",
  purple = "ps_type",
  brown = "growth_form",
  pink = "nitrogen_fixer",
  orange = "myco_is_am"
)

mod_colors <- c(swap_names(mod_params), Residuals = "grey")

lm_form <- paste0("value ~ ", paste(mod_params, collapse = " + "))

species_means <- raw_data %>%
  filter_at(mod_params, all_vars(!is.na(.))) %>%
  group_by(parameter, species_code, !!!rlang::syms(unname(mod_params))) %>%
  summarize(value = mean(Mean, na.rm = TRUE)) %>%
  ungroup()

species_fit <- species_means %>%
  group_by(parameter) %>%
  nest() %>%
  mutate(
    lmfit = map(data, lm, formula = formula(lm_form)),
    lm_anova = map(lmfit, car::Anova),
    tidy_fit = map(lmfit, broom::tidy),
    tidy_anova = map(lm_anova, broom::tidy)
  )

species_plot <- species_fit %>%
  unnest(tidy_anova) %>%
  group_by(parameter) %>%
  mutate(fraq_sq = sumsq / sum(sumsq)) %>%
  ungroup() %>%
  mutate(
    parameter = factor(parameter, variable_df$code) %>% lvls_revalue(variable_df$shortname),
    term = factor(term, rev(unique(term)))
  )

splot2 <- species_plot %>%
  select(parameter, term, p.value, fraq_sq) %>%
  mutate(is_signif = !is.na(p.value) & p.value < 0.1) %>%
  group_by(parameter) %>%
  mutate(term2 = fct_rev(term)) %>%
  arrange(parameter, term2) %>%
  mutate(
    hi = cumsum(fraq_sq),
    lo = hi - fraq_sq,
    pos = if_else(is_signif, (lo + hi) / 2, NA_real_),
  ) %>%
  select(-lo, -hi, -is_signif)

plt <- ggplot(splot2) +
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
if (interactive()) plt
ggsave(infile("manuscript/figures/across_species_anova.pdf"), plt)

############################################################

#raw_data %>%
  #summarize(f = 100 * mean(
    #!is.na(growth_form) &
      #!is.na(leaf_type) &
      #!is.na(myco_is_am) &
      #!is.na(phenology) &
      #!is.na(ps_type) &
      #!is.na(nitrogen_fixer)
      #)
    #)
