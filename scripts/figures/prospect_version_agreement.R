library(tidyverse)
library(rspecan)
library(metar)
import::from(scales, alpha)

results <- read_csv("spectra_db/cleaned_results.csv")

panel_line <- function(x, y, ...) {
  points(x, y, ...)
  fit <- lm(y ~ x)
  abline(a = 0, b = 1, lty = "dashed", col = "red")
  abline(fit, lty = "solid", col = "blue")
}

panel_cor <- function(x, y, digits = 2, prefix = "", ..., cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if (missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

result_pairs <- function(parameter, results, pdf = FALSE) {
  main <- variable_df$short_units[variable_df$code == parameter]

  dat_sub <- results %>%
    filter(parameter == !!parameter) %>%
    select(project_code, observation_id, prospect_version, Mean, `2.5%`, `97.5%`)

  dat_wide <- dat_sub %>%
    gather(variable, value, -(project_code:prospect_version)) %>%
    unite(temp, variable, prospect_version) %>%
    spread(temp, value)

  dat_pairs <- dat_wide %>%
    select(starts_with("Mean_")) %>%
    rename_all(str_remove, "Mean_")

  if (pdf) pdf(paste0("manuscript/figures/prospect_pairs_", parameter, ".pdf"))
  pairs(dat_pairs, lower.panel = panel_line, upper.panel = panel_cor,
        pch = 16, cex = 0.8, col = alpha("grey40", 0.1), main = main)
  if (pdf) dev.off()
}

parameters <- c("N", "Cab", "Car", "Cw", "Cm")
walk(parameters, result_pairs, results = results, pdf = TRUE)
