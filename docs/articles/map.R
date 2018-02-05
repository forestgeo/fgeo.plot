## ------------------------------------------------------------------------
library(fgeo.map)
library(fgeo.tool)
library(dplyr)
# Avoid conflict with `stats::filter()`
filter <- dplyr::filter

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
  # fig.show = "hold",
  fig.asp = 0.9
)

