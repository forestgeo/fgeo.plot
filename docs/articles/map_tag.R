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

