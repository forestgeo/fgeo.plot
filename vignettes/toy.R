## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
# hadley's settings
set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  echo = TRUE,  # {mine}
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  out.width = "100%",
  fig.align = "center",
  fig.width = 11,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold"
)

options(dplyr.print_min = 6, dplyr.print_max = 6)

## ------------------------------------------------------------------------
# install.packages("devtools")
# devtools::install_github(repo = "forestgeo/try@iss1")
library(try)

## ------------------------------------------------------------------------
str(toy_list)
lapply(toy_list, head)

## ------------------------------------------------------------------------
library(dplyr)
prepared <- prep_repulsive_tags(toy_list)

# Showing only the one quadrat to save space
str(prepared[1])
lapply(prepared[1], head)

## ------------------------------------------------------------------------
prep_repulsive_tags

## ------------------------------------------------------------------------
plot_list <- lapply_plot_repulsive_tags(prepared, site_name = "Toy data")
plot_list[[1]]

## ------------------------------------------------------------------------
lapply_plot_repulsive_tags(
  prepared, 
  site_name = "Common tweaks",
  point_shape = c(19, 1),
  point_size = 3,
  tag_size = 6,
  header = "My cool header, line 1: ......\nMy cool header line 2: ____",
  theme = ggplot2::theme_grey()
)[[1]]

## ------------------------------------------------------------------------
l1 <- pad(c("Checked by", "Date"))
l2 <- pad(c("Measured by", "Date"))
l3 <- pad("Notes", total_width = 40, pad = ".")
my_header <- get_header(l1, l2, l3)

lapply_plot_repulsive_tags(
  prepared, 
  site_name = "Tweaking header",
  header = my_header
)[[1]]

## ------------------------------------------------------------------------
my_theme <- get_theme(
  panel_grid_major_colour = "red",
  panel_grid_minor_colour = "blue", 
  panel_grid_minor_linetype = "dotted",
  panel_background_fill = "yellow", 
  plot_title_size = 40,
  plot_subtitle_size = 20, 
  legend_position = "bottom",
  axis_ticks = NULL, 
  axis_text_size = 10,
  legend_title = NULL
)

lapply_plot_repulsive_tags(
  prepared, 
  site_name = "Tweaking theme",
  theme = my_theme
)[[1]]

