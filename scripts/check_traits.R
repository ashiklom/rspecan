library(tidyverse)
library(rspecan)
import::from("progress", "progress_bar")
import::from("rlang", "sym")

dat <- get_metadata(indir("spectra_db"))

check_jitter <- function(dat, variable, pb = NULL) {
  if (!is.null(pb)) pb$tick()
  qvar <- sym(variable)
  dat_sub <- filter(dat, !is.na(!!qvar))
  p <- ggplot(dat_sub) +
    aes_string(x = "project_code", y = variable, color = "project_code") +
    geom_jitter()
  print(p)
}

traits_check <- dat %>% select(starts_with("leaf_")) %>% select_if(is.double) %>% colnames()

pb <- progress_bar$new(total = length(traits_check))
pdf("manuscript/figures/trait_summaries.pdf")
walk(traits_check, check_jitter, dat = dat, pb = pb)
dev.off()
