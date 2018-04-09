names_vec <- c(
  "# meso (spec)" = "N_mid",
  "Chl. tot. (spec)" = "Cab_mid",
  "Car. (spec)" = "Car_mid",
  "Anth. (spec)" = "Canth_mid",
  "Water (spec)" = "Cw_mid",
  "LDMC (spec)" = "Cm_mid",
  "LMA" = "leaf_mass_per_area",
  "Chl. tot." = "leaf_chltot_per_area",
  "Chl. a" = "leaf_chla_per_area",
  "Chl. b" = "leaf_chlb_per_area",
  "Car." = "leaf_cartot_per_area",
  "C" = "leaf_C_per_area",
  "N" = "leaf_N_per_area",
  "C:N" = "leaf_CN_ratio",
  "Water" = "leaf_water_thickness",
  "Lignin" = "leaf_lignin_per_area",
  "Cellulose" = "leaf_cellulose_per_area",
  "Protein" = "leaf_protein_per_area"
)

pairs_sub <- function(dat) {
  dat_names <- colnames(dat)
  names_sub <- names_vec[names_vec %in% dat_names]
  pairs_dat <- select_(dat, .dots = names_sub)
  corr_all <- cor(pairs_dat, use = "pairwise.complete.obs")
  corr_sub <- corr_all[, 1:6]
  has <- which(!is.na(corr_sub[, 1]))
  corr_sub[has, ]
}

corr_plot <- function(dat) {
  corr_sub <- pairs_sub(dat)
  print(corr_sub)
  corrplot::corrplot(corr_sub)
}

corr_plot_sp <- function(sp) {
  results %>%
    filter(USDA_code == sp) %>%
    not_missing() %>%
    corr_plot()
}

results %>% count(USDA_code, sort = TRUE)

corr_plot_sp("GLMA4")
corr_plot_sp("SOTU")
corr_plot_sp("QURU")

############################################################

pcplot <- function(mat, pc = c(1, 2)) {
  mat <- as.matrix(mat)
  cormat <- cor(mat, use = "pairwise.complete.obs")
  eig <- eigen(cormat)
  pred <- mat %*% eig$vectors
  xpts <- pred[, pc[1]]
  ypts <- pred[, pc[2]]
  xvec <- eig$vectors[, pc[1]]
  yvec <- eig$vectors[, pc[2]]
  xlim <- max(abs(xvec)) * c(-1, 1)
  ylim <- max(abs(xvec)) * c(-1, 1)
  plot(x = xpts, y = ypts, pch = 20)
  par(new = TRUE)
  plot(0, 0, type = "n", xlim = xlim, ylim = ylim, ann = FALSE, axes = FALSE)
  arrows(0, 0, xvec, yvec)
  text(xvec, yvec, colnames(pairs_soy))
}

## ---- pca_setup ----------

irm <- as.matrix(iris[,-5])
iris_pc <- princomp(irm, cor = TRUE)
iris_cor <- cor(irm)
iris_eig <- eigen(iris_cor)

pcpred <- irm %*% iris_eig$vectors
xvec <- iris_eig$vectors[, 1]
yvec <- iris_eig$vectors[, 2]
xlim <- max(abs(xvec)) * c(-1, 1)
ylim <- max(abs(yvec)) * c(-1, 1)
plot(pcpred[, 1], pcpred[, 2])
par(new = TRUE)
plot(0, 0, type = "n", xlim = xlim, ylim = ylim, ann = FALSE, axes = FALSE)
arrows(0, 0, x, y)

# PCA:
#   - PCA stdev = sqrt(eigen$values)
#   - PCA loadings = vectors (PC in columns; PC1 = eigen$vectors[, 1])
biplot(iris_pc)

pcbars <- function(dat) {
  corr <- corr_all[!nas, !nas]
  eig <- eigen(corr, symmetric = TRUE)
  vec <- eig$vectors
  var_names <- colnames(corr)
  rownames(vec) <- var_names
  nparam <- length(var_names)
  lseq <- seq_along(var_names)
  par(mfrow = c(1, 3), mar = c(0.1, 0.5, 2, 0.5))
  barplot(vec[, 1], names.arg = FALSE, horiz = TRUE, xaxt = "n", main = "PC1")
  plot(0, 0, type = "n", bty = "n", axes = FALSE, ann = FALSE, ylim = c(0, nparam))
  text(0, lseq - 0.5, rev(var_names))
  barplot(vec[, 2], names.arg = FALSE, horiz = TRUE, xaxt = "n", main = "PC2")
}

## ---- anova_setup ----------
specdb <- src_sqlite("~/Projects/prospect-traits/curated-leafspec/leaf_spectra.db")
species_attr <- tbl(specdb, "species") %>%
  left_join(tbl(specdb, "species_attributes")) %>%
  collect() %>%
  rename(USDA_code = speciescode) %>%
  select(-genus, -species, -variety)

results2 <- results %>%
  left_join(species_attr) %>%
  filter(
    !is.na(growth_form),
    !is.na(phenology),
    !is.na(N_mid)
  )

anova_var <- function(param) {
  form <- sprintf("%s ~ USDA_code", param)
  fit <- lm(as.formula(form), data = results2)
  anova_out <- car::Anova(fit)
  SS <- anova_out[, "Sum Sq"]
  SS_norm <- SS / sum(SS)
  SS_norm
}

lm_vars <- c(
  "Leaf type" = "leaf_type",
  "Leaf phenology" = "phenology",
  "Growth form" = "growth_form",
  "Myco. asso" = "myco_asso",
  "Shade tol." = "shade_tolerance",
  "Species" = "USDA_code"
)

lvls <- c(names(lm_vars), "Residual")
lvl_colors <- rev(c(RColorBrewer::brewer.pal(6, "Set1"), "grey"))
names(lvl_colors) <- rev(lvls)

param <- "Cab_mid"

anova_var <- function(param, lm_vars) {
  lm_lhs <- paste(lm_vars, collapse = " + ")
  lm_form <- as.formula(sprintf("%s ~ %s", param, lm_lhs))
  fit <- lm(lm_form, data = results2, na.action = na.omit)
  anova_fit <- car::Anova(fit)
  SS <- anova_fit[, "Sum Sq"]
  SS_norm <- SS / sum(SS)
  SS_norm
}

params <- c("N_mid", "Cab_mid", "Car_mid", "Canth_mid", "Cw_mid", "Cm_mid")
params_proper <- c("# meso", "Chl", "Car", "Anth", "Water", "LDMC")
anova_list <- lapply(params, anova_var, lm_vars)

anova_mat <- do.call(cbind, anova_list) %>%
  "colnames<-"(params_proper) %>%
  as_tibble() %>%
  mutate(attribute = factor(lvls, rev(lvls))) %>%
  gather(parameter, value, -attribute) %>%
  mutate(parameter = factor(parameter, params_proper))

## ---- anova_plot ----------
ggplot(anova_mat) +
  aes(x = parameter, y = value, fill = attribute) +
  geom_col() +
  ylab("Frac. variance explained") +
  scale_fill_manual(values = lvl_colors) +
  guides(fill = guide_legend(title = NULL)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 10)
  )
