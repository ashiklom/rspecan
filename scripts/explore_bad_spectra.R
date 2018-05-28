library(tidyverse)
library(rspecan)
library(PEcAnRTM)
import::from(metar, read_csvy)

results_raw <- read_csv("spectra_db/cleaned_results.csv")
md_raw <- read_csvy("spectra_db/cleaned_metadata.csvy") %>%
  mutate_if(is.character, na_if, "")

results_wide <- results_raw %>%
  filter(prospect_version == "D") %>%
  select(-specdb) %>%
  gather(stat, value, Mean:`97.5%`) %>%
  filter(stat %in% c("2.5%", "Mean", "97.5%")) %>%
  mutate(stat = recode(stat, "2.5%" = "lo", "97.5%" = "hi")) %>%
  unite(param_stat, parameter, stat) %>%
  spread(param_stat, value)
  
# Cedar creek biodiversity experiment does particularly badly at everything. Let's see why.
cc_data <- md_raw %>%
  filter(project_code == "ecosis_cedarcreek_biodiversity") %>%
  left_join(results_wide) %>%
  select_if(~any(!is.na(.)))

# Plot the fits of Cm against LMA
ggplot(cc_data) +
  aes(x = Cm_Mean, y = leaf_mass_per_area, xmin = Cm_lo, xmax = Cm_hi, color = species_code) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Do the same for Chlorophyll
ggplot(cc_data) +
  aes(x = Cab_Mean, y = leaf_chltot_per_area, xmin = Cab_lo, xmax = Cab_hi) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~species_code)

# Jitter of each trait
prospect_rxp <- "^N|Cab|Car|Canth|Cbrown|Cw|Cm|residual"
prospect_means <- sprintf("(%s)_Mean", prospect_rxp)
cc_data %>%
  select(species_code, matches(prospect_means)) %>%
  gather(parameter, value, -species_code) %>%
  ggplot(aes(x = species_code, y = value)) +
  geom_violin() +
  geom_jitter(size = 0.2) +
  facet_wrap(~parameter, scales = "free")

# They generally look pretty bad for all species.
# Is that just because of outliers with high error?

# First, calculate the error, sort species by mean error, and plot.
cc_diff <- cc_data %>%
  mutate(
    err = Cm_Mean - leaf_mass_per_area,
    species_code = fct_reorder(species_code, err, mean, na.rm = TRUE)
  )
ggplot(cc_diff) +
  aes(x = species_code, y = err, color = species_code) +
  geom_jitter()

# Looks like there are a handful of species that have a wide range of errors.
# We seem to be more likely to over-estimate LMA.
# Seems like a natural cut-off point for error is 0.01, so let's label those points.
cc_diff2 <- cc_diff %>%
  mutate(hi_error = err > 0.01)

# Now, let's plot these as a pairs plot to see if there are general features that emerge.
err_color <- if_else(cc_diff2$hi_error, "red", "green4")
cc_diff2 %>%
  select(matches("(N|Cab|Car|Canth|Cbrown|Cw|Cm)_Mean")) %>%
  pairs(col = err_color)

# Looks like all of the bad results have N values of exactly 1.
# Let's confirm with a ggplot.
cc_diff2 %>%
  filter(residual_Mean < 0.01) %>%
  select(hi_error, ends_with("Mean")) %>%
  gather(parameter, value, -hi_error) %>%
  ggplot() +
  aes(x = hi_error, y = value, fill = hi_error) +
  geom_jitter() +
  facet_wrap(~parameter, scales = "free")

# Indeed, all the N values look like more-or-less exactly 1.
# Let's have a look at the spectra.
cc_spec_data <- cc_diff2 %>%
  add_spectra_column("spectra_db")

bad_spec <- cc_spec_data %>%
  filter(hi_error) %>%
  pull_spectra()

good_spec <- cc_spec_data %>%
  filter(!hi_error) %>%
  pull_spectra()

matplot(good_spec, col = "green4", lty = "solid")
matplot(bad_spec, col = "red", lty = "solid", add = TRUE)

# Looks like all of the bad results are for spectra that give relatively low reflectance values across their range.
# Are there other distinguishing characteristics of those spectra?
# Let's look at traits.
cc_diff2 %>%
  select(hi_error, starts_with("leaf_")) %>%
  gather(trait, value, -hi_error) %>%
  ggplot() +
  aes(x = hi_error, y = value) +
  geom_jitter() +
  facet_wrap(~trait, scales = "free")

# Traits are pretty much the same, but spectra are much lower.
# Are there other attributes we could use to figure out differences?
cc_diff2 %>%
  select(
    -starts_with("leaf"),
    -dplyr::matches(prospect_rxp)
  ) %>%
  summarize_all(n_distinct) %>%
  glimpse()

# Not here. Looks like there are no meaningful contrasts.
# Let's try returning to the original data.
orig_file <- "raw_data/2014-cedar-creek-esr-grassland-biodiversity-experiment--leaf-level-contact-data--trait-predictions.csv"
orig_data <- read_csv(orig_file) %>%
  mutate(observation_id = sprintf("%s_%03d", "ecosis_cedarcreek_biodiversity", ID))

orig_data2 <- cc_diff2 %>%
  select(observation_id, hi_error) %>%
  left_join(orig_data)

# Again, let's look at possible contrasts.
many_contrasts <- orig_data2 %>%
  select(
    -dplyr::matches("^[[:digit:]]+$")
  ) %>%
  summarize_all(n_distinct) %>%
  select_if(~. > 1 & . < 700)
contrast_data <- orig_data2 %>%
  select(colnames(many_contrasts))

count_bad <- function(group, filt = TRUE) {
  group <- enquo(group)
  filt <- enquo(filt)
  contrast_data %>%
    filter(!!filt) %>%
    group_by(!!group) %>%
    summarize(fbad = mean(hi_error, na.rm = TRUE), n = n()) %>%
    arrange(desc(fbad))
}

count_bad(N_spp)

# Looks like I may have accidentally included spectra for multiple species?
# What happens if I compare single vs. multi-species spectra?
# EDIT: After looking at this again, `N_spp` is actually the number of species in the plot, not the number of species in the sample. So this is fine.
cc_diff2 %>%
  left_join(orig_data %>% select(-dplyr::matches("[[:digit:]]+"))) %>%
  mutate(multi_species = N_spp > 1) %>%
  select(multi_species, dplyr::matches(prospect_means)) %>%
  gather(parameter, value, -multi_species) %>%
  ggplot() +
  aes(x = multi_species, y = value) +
  geom_jitter() +
  facet_wrap(~parameter, scales = "free")

# Not quite the whole story. Still a bunch of N values that are 1.
# Let's look at more contrasts.

count_bad(Plot)
# Looks like a handful of plots perform significantly worse than others.
# Keep digging.

count_bad(Rep)
# Some of the replicates are significantly worse.
# But not sure if I can do anything with this. Moving on.

count_bad(`Sample Collection Date`)
# Bad results are mostly from June and July.

count_bad(`USDA Symbol`)
# Might be something here.
# Three species have >40% bad results.

# vim: set wrap :
