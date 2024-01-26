## ----preliminaries, echo=FALSE, results='hide', message=FALSE-----------------
# Using knitr for manuscript
library(knitr, quietly = TRUE)
opts_chunk$set(engine='R', tidy = FALSE, message = FALSE)
#render_sweave()

# JSS code formatting
#options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)

# Formatting
#options(scipen = 1, digits = 3)
Sys.setenv(LANG = 'en')

# RNG initialization
set.seed(10714)

# Required packages
library(SEI)
library(ggplot2)
library(dplyr)
library(xts)
library(zoo)
library(gridExtra)
#options(xts_check_TZ = FALSE)

## ----std_index_example--------------------------------------------------------
x <- rgamma(1000, shape = 2, scale = 1)
x_std <- std_index(x)

## ----std_index_ex_plot, dev='pdf', fig.width=9, fig.height=3, fig.align="center", out.width = "\\linewidth"----
plot_raw <- plot_sei(x, type = "hist", title = "Raw")
plot_std <- plot_sei(x_std, type = "hist", title = "Standardised")
grid.arrange(plot_raw, plot_std, nrow = 1)

## ----load_supply_data---------------------------------------------------------
data("data_supply", package = "SEI")
head(data_supply)

## ----subset_germany-----------------------------------------------------------
de_supply_h <- subset(data_supply, country == "Germany")
de_supply_h <- xts::xts(de_supply_h$PWS, de_supply_h$date) # convert to xts

## ----rescale------------------------------------------------------------------
de_supply_d <- xts::apply.daily(de_supply_h, "sum")    # daily data
de_supply_w <- xts::apply.weekly(de_supply_h, "sum")   # weekly data

## ----plot_raw_ts, dev='pdf', fig.width=9, fig.height=3, fig.align="center", out.width = "\\linewidth"----
lab <- "Renewable Energy Production (GWh)"
plot_h <- plot_sei(de_supply_h, lab = lab, title = "Hourly")
plot_d <- plot_sei(de_supply_d, lab = lab, title = "Daily")
plot_w <- plot_sei(de_supply_w, lab = lab, title = "Weekly")
grid.arrange(plot_h, plot_d, plot_w, nrow = 1)

## ----calculate_index----------------------------------------------------------
srepi_h <- std_index(de_supply_h)
srepi_d <- std_index(de_supply_d)
srepi_w <- std_index(de_supply_w, dist = "kde")

## ----use_rescale--------------------------------------------------------------
z <- std_index(de_supply_h, rescale = "days")
all.equal(srepi_d, z)

## ----plot_sei_ts, dev='pdf', fig.width=9, fig.height=3, fig.align="center", out.width = "\\linewidth"----
lab <- "SREPI"
ylims <- c(-3, 3)
plot_h <- plot_sei(srepi_h, lab = lab, ylims = ylims, title = "Hourly")
plot_d <- plot_sei(srepi_d, lab = lab, ylims = ylims, title = "Daily")
plot_w <- plot_sei(srepi_w, lab = lab, ylims = ylims, title = "Weekly")
grid.arrange(plot_h, plot_d, plot_w, nrow = 1)

## ----plot_dist, dev='pdf', fig.width=9, fig.height=3, fig.align="center", out.width = "\\linewidth"----
plot_raw <- plot_sei(de_supply_d, type = "hist",
                     lab = "Renewable Energy Production (GWh)")
plot_ind <- plot_sei(srepi_d, type = "hist", lab = "SREPI")
grid.arrange(plot_raw, plot_ind, nrow = 1)

## ----plot_dist_unif, dev='pdf', fig.width=9, fig.height=3, fig.align="center", out.width = "\\linewidth"----
srepi_d_prob <- std_index(de_supply_d, index_type = "probability")
plot_prob <- plot_sei(srepi_d_prob, type = "hist", lab = "SREPI",
                      ylims = c(0, 2), title = "Probability")
srepi_d_bnd <- std_index(de_supply_d, index_type = "bounded")
plot_bnd <- plot_sei(srepi_d_bnd, type = "hist", lab = "SREPI",
                     ylims = c(0, 1), title = "Bounded")
grid.arrange(plot_prob, plot_bnd, nrow = 1)

## ----define_droughts----------------------------------------------------------
thresholds <- -qnorm(c(0.9, 0.95, 0.975)) # -1.28, -1.64, -1.96
drought_df <- get_drought(srepi_d, thresholds, exceed = F)

## ----calculate event frequency------------------------------------------------
num_ev <- table(drought_df$ins)
names(num_ev) <- c("None", "Moderate", "Severe", "Extreme")
print(num_ev)

## ----calculate event duration-------------------------------------------------
table(drought_df$dur[drought_df$dur > 0])

## ----calculate event magnitude------------------------------------------------
mean(drought_df$mag[drought_df$mag != 0])

## ----load_wind_data-----------------------------------------------------------
data("data_wind_de", package = "SEI")
data_wind_de <- subset(data_wind_de, format(date, "%Y") >= 2017)
head(data_wind_de)

## ----rescale_wind, echo=FALSE-------------------------------------------------
de_wind_d <- xts::xts(data_wind_de$wsmean, data_wind_de$date) # convert to xts
de_wind_w <- xts::apply.weekly(de_wind_d, FUN = "mean")       # weekly data
de_wind_m <- xts::apply.monthly(de_wind_d, FUN = "mean")      # monthly data

## ----plot_raw_ts_wind, dev='pdf', fig.width=12, fig.height=4, fig.align="center", out.width = "\\linewidth"----
lab <- "Wind speed (m/s)"
ylims <- c(0, 8)
plot_ws_d <- plot_sei(de_wind_d, lab = lab, ylims = ylims, title = "Daily")
plot_ws_w <- plot_sei(de_wind_w, lab = lab, ylims = ylims, title = "Weekly")
plot_ws_m <- plot_sei(de_wind_m, lab = lab, ylims = ylims, title = "Monthly")
grid.arrange(plot_ws_d, plot_ws_w, plot_ws_m, nrow = 1)

## ----fit_ws_dists-------------------------------------------------------------
out_gamma <- fit_dist(data_wind_de$wsmean, dist = "gamma")
out_lnorm <- fit_dist(data_wind_de$wsmean, dist = "lnorm")
out_weibull <- fit_dist(data_wind_de$wsmean, dist = "weibull")

## ----aic----------------------------------------------------------------------
aic_vec <- c(out_gamma$fit_props['aic'],
             out_lnorm$fit_props['aic'],
             out_weibull$fit_props['aic'])
names(aic_vec) <- c("Gamma", "Log-normal", "Weibull")
print(aic_vec)

## ----ksp----------------------------------------------------------------------
ksp_vec <- c(out_gamma$fit_props['ks_pval'],
             out_lnorm$fit_props['ks_pval'],
             out_weibull$fit_props['ks_pval'])
names(ksp_vec) <- c("Gamma", "Log-normal", "Weibull")
print(round(ksp_vec, 4))

## ----ws_plot_dist_gam---------------------------------------------------------
x <- seq(0, 9, length.out = length(de_wind_d))
xlab <- "Wind speed (m/s)"
pars_gam <- out_gamma$params

plt_gam <- plot_sei(de_wind_d, type = "hist", lab = xlab, title = "Gamma") +
  geom_line(aes(x = x, y = dgamma(x, pars_gam[1], pars_gam[2])), col = "blue")

## ----ws_plot_dist, echo=FALSE, dev='pdf', fig.width=9, fig.height=3, fig.align="center", out.width = "\\linewidth"----
# Log-normal distribution
plt_lnorm <- plot_sei(de_wind_d, type = "hist", lab = xlab, title = "Log-normal")
plt_lnorm <- plt_lnorm + geom_line(aes(x = x, y = dlnorm(x, out_lnorm$params[1], out_lnorm$params[2])), col = "blue")

# Weibull distribution
plt_weib <- plot_sei(de_wind_d, type = "hist", lab = xlab, title = "Weibull")
plt_weib <- plt_weib + geom_line(aes(x = x, y = dweibull(x, out_weibull$params[1], out_weibull$params[2])), col = "blue")

grid.arrange(plt_gam, plt_lnorm, plt_weib, nrow = 1)

## ----get_std_indices----------------------------------------------------------
sei_ws_gam <- std_index(de_wind_d, dist = "gamma")
sei_ws_lnorm <- std_index(de_wind_d, dist = "lnorm")
sei_ws_weib <- std_index(de_wind_d, dist = "weibull")

## ----ws_ind_plot_dist, echo=FALSE, dev='pdf', fig.width=9, fig.height=3, fig.align="center", out.width = "\\linewidth"----
x <- seq(-3.5, 3.5, length.out = length(sei_ws_gam))
xlab <- "SWSI"
plt_gam <- plot_sei(sei_ws_gam, type = "hist", lab = xlab, title = "Gamma") +
  geom_line(aes(x = x, y = dnorm(x)), col = "blue")
plt_lnorm <- plot_sei(sei_ws_lnorm, type = "hist", lab = xlab, title = "Log-normal") +
  geom_line(aes(x = x, y = dnorm(x)), col = "blue")
plt_weib <- plot_sei(sei_ws_weib, type = "hist", lab = xlab, title = "Weibull") +
  geom_line(aes(x = x, y = dnorm(x)), col = "blue")
grid.arrange(plt_gam, plt_lnorm, plt_weib, nrow = 1)

