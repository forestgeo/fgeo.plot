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

options(dplyr.print_min = 6, dplyr.print_max = 6)

## ------------------------------------------------------------------------
library(dplyr)

# Fixing wrong names
vft <- rename(bciex::bci12vft_mini, QX = x, QY = y)

# Filter the data you want. For example:

# Filtering trees of diameter greater than 10 cm from the last census of plot 1
# (see also ?rm_dead_twice)
want <- dplyr::filter(
  vft,
  DBH > 10,
  PlotCensusNumber == max(PlotCensusNumber, na.rm = TRUE),
  PlotID == 1
)

# Filtering only two quadrats to save time
two_quadrats <- top(want, QuadratID, 2)
p <- map_quad(two_quadrats)

# Visualizing only the first plot of `p`
first(p)

# Printing all plots pf `p` to .pdf, with parameters optimized for size letter
tmp <- tempfile()  # Remplace this by somehtihing like "maps.pdf"

pdf(tmp, paper = "letter", height = 10.5, width = 8)
p
dev.off()

# Customizing the maps ----------------------------------------------------

# Filtering only one quadrat to save time
one_quad <- top(want, QuadratID)

# A custom title and header
myheader <- paste(
  " ",
  "Head column 1                     Head column 2                          ",
  " ",
  " ........................................................................",
  " ........................................................................",
  sep = "\n"
)
map_quad(one_quad, title_quad = "My Site, 2018. Quad:", header = myheader)

# Many more tweaks are possible

# Use functions to tweak theme
library(ggplot2)

map_quad(
  one_quad, 
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

