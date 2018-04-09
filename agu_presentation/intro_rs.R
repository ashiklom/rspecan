## ---- rs_setup ----------
library(PEcAnRTM)
library(tidyverse)
data(dataSpec_prospectd)
data(testspec)

## ---- sig_setup ----------
pars <- c(
  "Carotenoids" = "k_car",
  "Water" = "k_cw",
  "Chlorophyll" = "k_cab",
  "Anthocyanins" = "k_canth",
  "Dry matter" = "k_cm"
)
cols <- RColorBrewer::brewer.pal(length(pars), "Set1")
ys <- dataSpec_prospectd[, pars]
ys <- apply(ys, 2, function(x) x / max(x))
par_sig <- list(mar = c(2.1, 2.1, 0.2, 0.2), cex = 0.6,
                cex.axis = 0.7,
                mgp = c(1, 0.3, 0), tcl = -0.3)

## ---- sig_plot ----------
par(par_sig)
matplot(dataSpec_prospectd[, "wavelength"], ys,
        type = "l", lty = "solid", col = cols, lwd = 3,
        xlab = "Wavelength", ylab = "Relative absorptivity")
legend(
  "top",
  legend = names(pars),
  lty = "solid",
  lwd = 3,
  col = cols
)

## ---- spec_plot ----------
par(par_sig)
wl <- 400:2500
refl <- testspec_ACRU
matplot(wl, refl, type = "l", col = "black", lty = "solid", lwd = 0.5,
        xlab = "Wavelength", ylab = "Leaf reflectance")

## ---- rtm_setup----------
prospect_d <- function(x, param) {
  pars <- defparam("prospect_d")
  pars[param] <- x
  prospect(pars, "D")[, 1]
}
sens <- function(param, vec, col, ...) {
  m <- vapply(vec, prospect_d, numeric(2101), param = param)
  matplot(400:2500, m, type = "l", lty = "solid", lwd = 1, col = col, ...)
}
leg <- function(vec, col, i = c(1, 4, 7), ...) {
  legend(
    "topright",
    legend = vec[i],
    col = col[i],
    lty = "solid",
    lwd = 2,
    ...
  )
}

N_vec <- c(1, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7, 3.0)
N_cols <- colorRampPalette(c("grey", "purple"))(length(N_vec))
chl_vec <- c(10, 15, 20, 25, 30, 40, 50, 70, 90, 120)
chl_cols <- colorRampPalette(c("grey", "forestgreen"))(length(chl_vec))
cw_vec <- c(0.001, 0.003, 0.004, 0.005, 0.01, 0.03, 0.05, 0.08)
cw_cols <- colorRampPalette(c("grey", "deepskyblue"))(length(cw_vec))

## ---- rtm_plots ----------
par(oma = c(0, 1, 0.2, 0))
screen2 <- split.screen(c(1, 3), rtm_screen)
parl <- list(mai = c(0.4, 0.2, 0.2, 0.01),
             mgp = c(3, 0.1, 0),
             cex = 0.6, tcl = -0.1,
             cex.axis = 0.7, cex.main = 0.9)
screen(screen2[1])
par(parl)
sens("N", N_vec, N_cols, xlab = "", ylab = "",
     main = expression("Mesophyll layers"))
leg(N_vec, N_cols, cex = 0.6)
screen(screen2[2])
par(parl)
sens("Cab", chl_vec, chl_cols, xlab = "", ylab = "",
     main = expression("Chlorophyll" ~ (mu*g ~ cm^{-2})))
leg(chl_vec, chl_cols, cex = 0.6)
screen(screen2[3])
par(parl)
sens("Cw", cw_vec, cw_cols, xlab = "", ylab = "",
     main = expression("Water content" ~ (g ~ cm^{-2})))
leg(cw_vec, cw_cols, cex = 0.6)
screen(rtm_screen)
mtext("Wavelength (nm)", side = 1, line = 4, cex = 0.6)
mtext("Simulated leaf reflectance", side = 2, line = 4, cex = 0.4)

## ---- rtm_inversion ----------
obs <- testspec_ACRU[, 1]
mod <- function(x) prospect(x, "D")[, 1]
fit <- invert.lsq(obs, defparam("prospect_d"), mod)
goodpars <- fit$par[-5]
par_names <- c("# meso", "Chl.", "Car.", "Anth.", "Water", "LDMC")
badpars <- c(1.8, 3, 20, 10,  0.003, 0.01)
refpars <- c(2.0, 100, 50, 15, 0.01, 0.012)
goodref <- goodpars / refpars
badref <- badpars / refpars
names(goodref) <- names(badref) <- par_names
fit_par <- list(mar = c(2, 2, 1.5, 0.5), cex = 0.5, mgp = c(2, 0.3, 0), tcl = -0.3,
                cex.axis = 0.6, cex.lab = 0.8, cex.main = 0.9)
bar_par <- list(cex = 0.4, cex.lab = 0.9, cex.axis = 0.8,
                mar = c(0.3, 2.6, 0, 0), mgp = c(1, 0.3, 0), tcl = -0.3, las = 1)
obs_col <- "grey30"
good_col <- "forestgreen"
bad_col <- "red"
mod_col <- "purple"
lwd <- 1.5

## ---- observed_spec ----------
wl <- 400:2500
refl <- testspec_ACRU[, 1]
par(mar = c(2, 2, 1.5, 0.1), cex = 0.6, cex.title = 0.9, cex.axis = 0.8,
    mgp = c(2, 0.3, 0), tcl = -0.3)
plot(refl ~ wl, type = "l",
     xlab = "Wavelength (nm)", ylab = "Reflectance", main = "Observed")

## ---- good_fit ----------
goodprosp <- c(goodpars[c("N", "Cab", "Car", "Canth")], 0, goodpars[c("Cw", "Cm")])
good_refl <- mod(goodprosp)
par(fit_par)
plot(wl, refl, type = "l", col = obs_col, lty = "solid",
     xlab = "Wavelength", ylab = "Reflectance", ylim = range(c(refl, good_refl)))
lines(wl, good_refl, col = mod_col, lty = "dashed", lwd = lwd)
legend("topright", c("Obseved", "Modeled"), lty = "solid", col = c(obs_col, mod_col),
       cex = 0.5)

## ---- bad_fit ----------
badprosp <- c(badpars[1:4], 0, badpars[5:6])
bad_refl <- mod(badprosp)
par(fit_par)
plot(wl, refl, type = "l", col = obs_col,
     xlab = "Wavelength", ylab = "Reflectance", ylim = range(c(refl, bad_refl)))
lines(wl, bad_refl, col = mod_col, lty = "dashed", lwd = lwd)
legend("topright", c("Obseved", "Modeled"), lty = "solid", col = c(obs_col, mod_col),
       cex = 0.5)

## ---- bad_bar ----------
par(bar_par)
barplot(rev(badref), col = "red3",
        ylab = "", xlab = "", horiz = TRUE)

## ---- good_bar ----------
par(bar_par)
barplot(rev(goodref), col = "green3",
        ylab = "", xlab = "", horiz = TRUE)

## ---- normaldist ----------
par(mar = rep(0, 4))
plot(dnorm, -3, 3, type = "l", lwd = 2,
     ann = FALSE, xaxt = "n", yaxt = "n", frame.plot = FALSE)
