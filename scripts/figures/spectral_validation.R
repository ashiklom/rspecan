library(tidyverse)
library(rspecan)
library(PEcAnRTM)
import::from(metar, read_csvy)
import::from(progress, progress_bar)

# Issues with this script:
#  - `cbind.spectra` needs to sort by wavelength before combining. Otherwise, waves get jumbled.
#  - `cbind.spectra` and PROSPECT both need to spit out / preserve spectra types
#  - Need to figure out how to deal with spectra at different resolutions. 
#  Perhaps downsample them? Or, stick to lowest common denominator for all spectra?

dat_wide <- readRDS("spectra_db/cleaned_wide.rds")

if (!file.exists("spectra_db/all_errors.rds")) {
  dat_spec <- dat_wide %>%
    add_spectra_column("spectra_db")

  do_prospect <- function(N, Cab, Car, Canth, Cbrown, Cw, Cm, ..., pb = NULL) {
    on.exit(if (!is.null(pb)) pb$tick())
    pars <- c(N, Cab, Car, Canth, Cbrown, Cw, Cm)
    pout <- PEcAnRTM::prospect(pars, "D")
    pa <- log10(1 / pout[, 1])
    PEcAnRTM::spectra(cbind(pout, pa), spectra_types = c("R", "T", "PA"))
  }

  prosp_error <- function(obs, sim, pb = NULL) {
    on.exit(if (!is.null(pb)) pb$tick())
    wobs <- PEcAnRTM::wavelengths(obs)
    stobs <- PEcAnRTM::spectra_types(obs)
    obs <- obs[wobs > 400, ]
    wobs <- PEcAnRTM::wavelengths(obs)

    stsim <- PEcAnRTM::spectra_types(sim)
    rsim <- PEcAnRTM::resample(sim, wobs)
    colnames(rsim) <- stsim
    rsim <- rsim[, stobs]
    rsim - obs
  }

  dat_spec2 <- dat_spec %>%
    mutate(spectra_types = map(spectra, PEcAnRTM::spectra_types))

  nspec <- map_int(dat_spec2$spectra, ncol)
  spectypes <- reduce(dat_spec2$spectra_types, c)
  obs_ids <- map2(dat_spec2$observation_id, nspec, rep) %>% reduce(c)

  dat_prosp <- dat_spec2 %>%
    mutate(sim_spec = pmap(., do_prospect, pb = pb))

  pb <- progress_bar$new(total = nrow(dat_prosp))
  dat_err <- dat_prosp %>%
    mutate(err_spec = map2(spectra, sim_spec, prosp_error, pb = pb))

  err_split <- split(dat_err$err_spec, rep(1:100, length.out = nrow(dat_err)))
  pb <- progress_bar$new(total = length(err_split), format = "[:bar] :percent :eta")
  err1 <- map(err_split, ~{pb$tick(); do.call(cbind, .)})
  all_errors <- do.call(cbind, err1)
  attr(all_errors, "spectra_types") <- spectypes
  colnames(all_errors) <- obs_ids

  saveRDS(all_errors, file = "spectra_db/all_errors.rds")
} else {
  all_errors <- readRDS("spectra_db/all_errors.rds")
}

summary_error <- function(x) {
  win <- wavelengths(x)
  wout <- seq(405, 2500, 10)
  mu <- rowMeans(x, na.rm = TRUE) %>% resample(win, wout)
  lo <- apply(x, 1, quantile, 0.025, na.rm = TRUE) %>% resample(win, wout)
  hi <- apply(x, 1, quantile, 0.975, na.rm = TRUE) %>% resample(win, wout)
  cbind(mu, lo, hi)
}

st <- PEcAnRTM::spectra_types(all_errors)
all_errors <- all_errors[owaves, ]
waves <- wavelengths(all_errors)

waves <- seq(405, 2500, 10)
par(mfrow = c(4, 1))
all_errors[, st == "PA"] %>% summary_error %>% matplot(x = waves, type = "l")
abline(h = 0, lty = "dashed")
all_errors[, st == "R"] %>% summary_error %>% matplot(x = waves, type = "l")
all_errors[, st == "T"] %>% summary_error %>% matplot(x = waves, type = "l")
plot(prospect(defparam("prospect_d"), "D")[, 1])
