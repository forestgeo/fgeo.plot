## ----pkg-hide, echo = FALSE, message=FALSE, warning=FALSE----------------
# install_github("forestgeo/map")
library(map)
library(fgeo.utils)
library(dplyr)

# Print only a few rows of data framed to save time and space
options(dplyr.print_min = 6, dplyr.print_max = 6)

## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
# hadley's settings
set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "98%",
  fig.align = "center",
  fig.width = 7.5, 
  fig.asp = 0.9,
  fig.show = "hold"
)

## ------------------------------------------------------------------------
map_tag(bci_vft1_rnm, title_quad = "BCI 2012", move_edge = 0.4)[4]

## ------------------------------------------------------------------------
map_tag(bci_vft1_rnm, 
  title_quad = "BCI 2012", x_q = 10, x_sq = 2.5, 
  # if not extended, the lines surrounding the map won't plot
  move_edge = 0.25
)[1]

## ----bigger--------------------------------------------------------------
# Creating new data set with qx and qy ranging 0-100
bigger <- bci_vft1_rnm
n <- nrow(bigger)
bigger$qx <- sample(0:100, n, replace = TRUE)
bigger$qy <- sample(0:100, n, replace = TRUE)

map_tag(
  bigger, 
  x_q = 100, x_sq = 25, 
  move_edge = -1.75
)[1]

## ----smaller-------------------------------------------------------------
# Creating new data set with qx and qy ranging 0-100
smaller <- bci_vft1_rnm
n <- nrow(smaller)
smaller$qx <- sample(0:10, n, replace = TRUE)
smaller$qy <- sample(0:10, n, replace = TRUE)

map_tag(smaller, x_q = 10, x_sq = 2.5, move_edge = 0.25)[1]

