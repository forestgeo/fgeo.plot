## ----setup, include=FALSE------------------------------------------------

# > If you want to make sure the font size is consistent across all your 
# figures, whenever you set out.width, you'll also need to adjust fig.width to 
# maintain the same ratio with your default out.width. For example, if your 
# default fig.width is 6 and out.width is 0.7, when you set out.width = "50%" 
# you'll need to set fig.width to 4.3 (6 * 0.5 / 0.7).
# --http://r4ds.had.co.nz/

set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "90%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold"
)

options(dplyr.print_min = 6, dplyr.print_max = 6)

## ----pkg-----------------------------------------------------------------
# Useful packages
library(ggplot2)
library(dplyr)

## ----cns_data_sub--------------------------------------------------------
# Data
library(bci)
# Data for a minimal example.
cns_data <- bci::bci12full7
# Filtering 3 species only
species_selected <- unique(cns_data$sp)[1:3]
cns_data_sub <- filter(cns_data, sp %in% species_selected)
# Sampling only some rows
cns_data_sub <- sample_n(cns_data_sub, 10000)

## ----plot-limits---------------------------------------------------------
xlimits <- c(0, max(cns_data$gx, na.rm = TRUE))
ylimits <- c(0, max(cns_data$gy, na.rm = TRUE))

## ----plot-sp-------------------------------------------------------------
plot_sp <- function(cns_data_sub) {
  ggplot(data = cns_data_sub, aes(x = gx, y = gy)) +
    geom_point() +
    facet_grid(. ~ sp) +
    coord_fixed(xlim = xlimits, ylim = ylimits) +
    theme_bw()
}

## ----plot-list-----------------------------------------------------------
split_by_sp <- split(cns_data_sub, cns_data_sub$sp)
plot_list <- lapply(split_by_sp, plot_sp)

## ----show-saved, fig.align="default", out.width = "30%", fig.widh = (6 * 0.3 / 0.9)----
plot_list

## ----show-quick-default, fig.align="default", out.width="30%", fig.widh=(6 * 0.3 / 0.9)----
# The package try is where I try new things that I may or may not develop.
library(try)

# Selecting all species in the example dataset
all_species <- unique(cns_data_sub$sp)
lapply_plot_sp(species = all_species, cns_data = cns_data_sub)

## ----show-quick-tweaked, fig.align="default", out.width="30%", fig.widh=(6 * 0.3 / 0.9)----
lapply_plot_sp(
  species = all_species, cns_data = cns_data_sub,
  # Change the look of the points  passed to ?geom_point
  color = "blue", size = 3, shape = 1
)

