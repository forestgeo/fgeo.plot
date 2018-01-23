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
# Example data
census <- bciex::bci12t7mini
species <- c("hybapr", "faraoc")

# Defaults
p <- map_sp(census, species)
# Visualizing only the first plot of `p`
first(p)
# Printing all plots of `p` to .pdf, with parameters optimized for size letter
pdf("map.pdf", paper = "letter", height = 10.5, width = 8)
p
dev.off()

# Simple tweaks
p <- map_sp(
  census, species,
  # Passed to ggplot2::geom_point()
  size = 4, shape = 22, fill = "green", colour = "black", stroke = 2
)
first(p)

# Add elevation and tweak lines
# Fixing wrong names of elevation data
elevation <- rename(bciex::bci_elevation, gx = x, gy = y)
p <- map_sp(
  census, species,
  elevation = elevation, line_size = 1, low = "red", high = "blue", bins = 10
)
first(p)

# Dealing with overplotting
crowded <- tibble(
  sp = sample(c("species1"), 10000, replace = TRUE),
  gx = sample.int(1000, 10000, replace = TRUE),
  gy = sample.int(500, 10000, replace = TRUE)
)
map_sp(crowded, c("species1"))
# Less overplotting
map_sp(crowded, c("species1"), size = 1, alpha = 5/10, shape = 21)

# Limits
p <- map_sp(census, species, xlim = c(0, 1500), ylim = c(0, 1000))
first(p)

# Themes
library(ggplot2)
# Using pre-made themes
p <- map_sp(census, species, theme = ggplot2::theme_classic())
first(p)
# Tweaking the default theme of map_sp()
small_tweak <- theme_map_sp(
  text = element_text(size = 30, face = "bold.italic")
)
p <- map_sp(census, species, theme = small_tweak)
first(p)
large_tweak <- theme(
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(size = 8, colour = "red"),
  text = element_text(size = 11, face = "bold.italic", colour = "white"),
  plot.background = element_rect(fill = "black"),
  plot.margin = margin(2, 2, 2, 2, "cm"),
  strip.background = element_rect(fill = "darkgreen"),
  strip.text = element_text(colour = "white"),
  panel.background = element_rect(fill = "lightgreen"),
  panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
  panel.grid.major = element_line(colour = "black")
)
p <- map_sp(census, species, theme = large_tweak)
first(p)

# Multiple maps per page
library(gridExtra)
four_species <- c("hybapr", "faraoc", "des2pa", "tri2tu")
p <- map_sp(census, four_species)
multipaged <- marrangeGrob(p, nrow = 1, ncol = 2)
# Printing all plots of `p` to .pdf, with parameters optimized for size letter
# Option 1
pdf("map.pdf", paper = "letter", height = 10.5, width = 8)
multipaged
dev.off()
# Option 2
ggsave("map.pdf", multipaged, height = 10.5, width = 8)

# Extending with ggplot2
p0 <- map_sp(census, species)
#  Adding new layer to one element of the plots' list
p0[["hybapr"]] + geom_vline(aes(xintercept = 300), colour = "red")
# Adding new layer to all elements of the plots' list
# * Adding a vertical line
p1 <- lapply(p0, `+`, geom_vline(aes(xintercept = 300), colour = "red"))
marrangeGrob(p1, nrow = 2, ncol = 1)
# * Also adding a horizontal line
p2 <- lapply(p1, `+`, geom_hline(aes(yintercept = 400), colour = "blue"))
marrangeGrob(p2, nrow = 2, ncol = 1)

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

## ------------------------------------------------------------------------
# Filtering the data to map -----------------------------------------------

# Filter the data you want. For example:
# (see ?top1quad)
want <- filter(
  top1quad,
  CensusID == 6,
  PlotID == 1
)
p <- map_tag(want)
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
first(map_tag(dead))
# * If you filter by `DBH`, you loose the dead trees becaue their `DBH = NA`
wrong <- filter(dead, DBH > 10)
wrong
# * The right way to do it is to explicietly inlcude rows where DBH = NA
right <- filter(dead, DBH > 10 | is.na(DBH))
right
first(map_quad(right))

# Keeping dead trees with `is.na(DBH)` (e.g. tag 127885.d on the bottom right)
p <- filter(top4quad, DBH > 20 | is.na(DBH))
first(map_tag(p))

# For more complex filtering, see also ?fgeo.tool::discard_dead_twice



# Customizing the maps ----------------------------------------------------

# Common tweaks

p <- map_tag(top1quad, show_page = FALSE, show_subquad = FALSE)
first(p)

p <- map_tag(
  top1quad,
  title_quad = "BCI 2012. Quadrat: ",
  bl = "bottom-left", br = "bottom-right", tr = "top-right", tl = "top-left",
  header = "Line 1: _________\nLine 2:\nLine 3:.....................",
  subquad_offset = -1,
  point_size = 3, point_shape = c(17, 6),
  tag_size = 2,
  move_edge = 0.5
)
first(p)



# Themes

library(ggplot2)

p <- map_tag(top1quad, theme = theme_gray())
first(p)

# Tweaking the default theme of map_tag()

small_tweak <- theme_map_tag(legend.position = "bottom")
p <- map_tag(top1quad, theme = small_tweak)
first(p)

large_tweak <- theme(
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(size = 8, colour = "red"),
  text = element_text(size = 11, face = "bold.italic", colour = "white"),
  plot.background = element_rect(fill = "black"),
  plot.margin = margin(2, 2, 2, 2, "cm"),
  strip.background = element_rect(fill = "darkgreen"),
  strip.text = element_text(colour = "white"),
  # make grid to dissapear by matching background colour
  panel.background = element_rect(fill = "lightgreen"),
  panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
  panel.grid.major = element_line(colour = "black")
)
p <- map_tag(top1quad, theme = large_tweak)
first(p)



# Quadrat and subquadrat dimensitons

# Adapting the dimensions of quadrat and subquadrat to the range of qx and qy
# Creating new data set with QX and QY ranging 0-100
smaller <- top1quad
n <- nrow(smaller)
smaller$QX <- sample(0:10, n, replace = TRUE)
smaller$QY <- sample(0:10, n, replace = TRUE)

p <- map_tag(smaller, x_q = 10, x_sq = 2.5)
first(p)

# If limit-lines aren't visible, try extending the grid a little
p <- map_tag(smaller, x_q = 10, x_sq = 2.5, move_edge = 0.25)[1]
first(p)

