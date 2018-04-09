library(tidyverse)
library(units)
library(ggforce)
library(rspecan)

dat <- readRDS("processed_results/complete_results.rds") %>%
  filter(project != "ngee_tropics")

# Summary plots for each PROSPECT parameter
dat_long <- dat %>%
  select(project, USDA_code, matches("^(N|Cab|Car|Canth|Cw|Cm)_mid")) %>%
  gather(parameter, value, -project, -USDA_code)

ggplot(dat_long) +
  aes(x = project, y = value, fill = project, color = project) +
  geom_violin() +
  facet_wrap(~parameter, scales = "free") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank()
  )

if (FALSE) {
  weird_ids <- dat %>%
    #filter(N_mid > set_units(6, units::unitless)) %>%
    select(data_name, project, spectra_id) %>%
    print(n = Inf)

  getspec <- function(data_name, spectra_id) {
    stopifnot(is.character(spectra_id))
    fname <- file.path("processed_data", paste0(data_name, ".rds"))
    stopifnot(file.exists(fname))
    all_spec <- readRDS(fname)$spectra
    wl <- getwl(all_spec)
    cbind(wl = wl, refl = all_spec[, spectra_id])
  }

  weird_spec <- weird_ids %>%
    mutate(spectra = map2(data_name, spectra_id, getspec))

  weird_spec %>%
    mutate(spectra = map(spectra, as_tibble)) %>%
    unnest(spectra) %>%
    ggplot() +
    aes(x = wl, y = refl, group = interaction(project, spectra_id), color = project) +
    geom_line()
}
