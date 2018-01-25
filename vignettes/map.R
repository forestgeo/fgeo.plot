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

## ------------------------------------------------------------------------
# Filter the data you want. For example:
# (see ?top4quad)
want <- filter(
  top4quad,
  CensusID == 6,
  PlotID == 1
)
p <- map_quad(want)
# Visualizing only the first plot of `p`
first(p)
# Printing all plots of `p` to .pdf, with parameters optimized for size letter
pdf("map.pdf", paper = "letter", height = 10.5, width = 8)
p
dev.off()

# Be careful if filtering by DBH: You may unintentionally remove dead trees.
# * Confirm this dataset has dead trees:
# (see `?top4quad`)
dead <- top4quad %>%
  add_status_tree(status_a = "alive", status_d = "dead") %>%
  top(QuadratID) %>%
  filter(status_tree == "dead")
select(dead, Tag, Status, status_tree, DBH)
map_quad(dead)
# * If you filter by `DBH`, you loose the dead trees becaue their `DBH = NA`
wrong <- filter(dead, DBH > 10)
map_quad(wrong)
# * The right way to do it is to explicietly inlcude rows where DBH = NA
right <- filter(dead, DBH > 10 | is.na(DBH))
map_quad(right)

# Keeping dead trees with `is.na(DBH)` (e.g. tag 127885.d on the bottom right)
p <- filter(top4quad, DBH > 20 | is.na(DBH))
first(map_quad(p))

# For more complex filtering, see also ?fgeo.tool::discard_dead_twice)

# Customizing the maps ----------------------------------------------------

# A custom title and header
myheader <- paste(
  " ",
  "Head column 1                     Head column 2                          ",
  " ",
  " ........................................................................",
  " ........................................................................",
  sep = "\n"
)
# See ?top1quad
map_quad(top1quad, title_quad = "My Site, 2018. Quad:", header = myheader)

# Tweak the theme with ggplot
library(ggplot2)

map_quad(
  top1quad,
  title_quad = "My Site, 2018. Quad:",
  header = map_quad_header("spanish"),
  tag_size = 3,
  theme = theme_map_quad(
    axis.text = NULL,  # NULL shows axis.text; element_blank() doesn't.
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 5),
    panel.background = element_rect(fill = "grey")
  )
)

