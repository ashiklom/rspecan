library(tidyverse)
library(ggforce)
library(corrplot)
dat <- readRDS("processed_results/complete_results.rds")

pairs_dat <- dat %>%
  select(N_mid, Cab_mid, Canth_mid, Car_mid, Cw_mid, Cm_mid,
         leaf_mass_per_area, leaf_CN_ratio, leaf_C_per_area,
         leaf_chla_per_area, leaf_chlb_per_area,
         leaf_N_per_area, leaf_H_per_area,
         leaf_fiber_per_area, leaf_lignin_per_area, leaf_cellulose_per_area,
         leaf_protein_per_area)
cmat <- cor(as.matrix(pairs_dat), use = "pairwise.complete.obs")
corrplot(cmat)
