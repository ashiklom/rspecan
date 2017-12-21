## ---- setup ----------
library(knitr)
library(rspecan)
library(cowplot)
spectra_file <- here::here("processed_data", "spectra.h5")
results_file <- here::here("processed_results", "complete_results.rds")
opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  cache = 2
)
set_alias(w = "fig.width", h = "fig.height")
results_raw <- readRDS(results_file) %>%
  filter(!(target_type %in% c("npv", "rock", "soil")))
variable_df <- tribble(
  ~code, ~shortname, ~longname, ~name_with_unit,
  "N_mid", "# meso", "Number of mesophyll layers", "Number of mesophyll layers",
  "Cab_mid", "Chl.", "Chlorophyll", "Chlorophyll ~ (mu * g ~ cm ^ {-2})",
  "Car_mid", "Car.", "Carotenoids", "Carotenoids ~ (mu * g ~ cm ^ {-2})",
  "Canth_mid", "Anth.", "Anthocyanins", "Anthocyanins ~ (mu * g ~ cm ^ {-2})",
  "Cw_mid", "Water", "Water", "Water ~ (g ~ m ^ {-2})",
  "Cm_mid", "LDMC", "Dry matter", "Dry matter ~ (g ~ m ^ {-2})"
)

