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

## ----load_ic------------------------------------------------------------------
data("data_ic2017", package = "SEI")
head(data_ic2017)

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

## ----calculate_index, warning=FALSE-------------------------------------------
srepi_h <- std_index(de_supply_h)
srepi_d <- std_index(de_supply_d)
srepi_w <- std_index(de_supply_w)

## ----use_rescale--------------------------------------------------------------
z <- std_index(de_supply_h, rescale = "days")
all.equal(srepi_d, z)

## ----plot_sei_ts, dev='pdf', fig.width=9, fig.height=3, fig.align="center", out.width = "\\linewidth"----
lab <- "SREPI"
plot_h <- plot_sei(srepi_h, lab = lab, title = "Hourly")
plot_d <- plot_sei(srepi_d, lab = lab, title = "Daily")
plot_w <- plot_sei(srepi_w, lab = lab, title = "Weekly")
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
num_ev <- sapply(0:3, function(i) sum(drought_df$ins == i))
names(num_ev) <- c("None", "Moderate", "Severe", "Extreme")
print(num_ev)

## ----calculate event duration-------------------------------------------------
table(drought_df$dur[drought_df$dur > 0])

## ----calculate event magnitude------------------------------------------------
mean(drought_df$mag[drought_df$mag != 0])

