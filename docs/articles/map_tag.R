## ----pkg-hide, echo = FALSE, message=FALSE, warning=FALSE----------------
# install_github("forestgeo/map")
library(map)
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

## ----hide-sinh, echo=FALSE-----------------------------------------------
filter_first_quadrat <- function(vft) {
  first_quadrat <- unique(vft$QuadratName)[1]
  vft[vft$QuadratName == first_quadrat, ]
}
some_site_vft <- bciex::bci12vft_mini %>%
  # BCI has older names that must be fixed to work with map_tag()
  rename(QX = x, QY = y) %>% 
  filter_first_quadrat()

## ------------------------------------------------------------------------
glimpse(some_site_vft)

## ------------------------------------------------------------------------
some_site_vft1 <- filter(some_site_vft, PlotID == 1)

## ------------------------------------------------------------------------
# Using a private data set; and using only one quadrat for a small example
maps <- map_tag(some_site_vft1)

## ----yose-print-screen---------------------------------------------------
maps[1]

## ----yose-print-pdf------------------------------------------------------
pdf("example-some_site.pdf", paper = "a4")
maps
dev.off()

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("example-some_site.pdf")

## ----sho-chunk-that-was-hidden-above-------------------------------------
# install_github("forestgeo/map")
library(map)
library(dplyr)

# Print only a few rows of data framed to save time and space
options(dplyr.print_min = 6, dplyr.print_max = 6)

## ----data-vtf------------------------------------------------------------
# Subset of a public ViewFullTable from BCI (source:
# https://repository.si.edu/handle/10088/20925).

# Convert to tibble (modern dataframe) for better printing
bci_vft <- as_tibble(bciex::bci12vft_mini)
glimpse(bci_vft)

## ------------------------------------------------------------------------
bci_vft1 <- filter(bci_vft, PlotID == 1)

## ----rename-variables, error=TRUE----------------------------------------
# Fails
map_tag(bci_vft1)

## ----fix-names-----------------------------------------------------------
bci_vft1_rnm <- dplyr::rename(bci_vft1, qx = x, qy = y)
# Using lowercase names for simiplicity
names(bci_vft1_rnm) <- tolower(names(bci_vft1_rnm))

## ----filter-bci----------------------------------------------------------
any_quadrat <- sample(unique(bci_vft1_rnm$quadratname), 1)
filter(bci_vft1_rnm, quadratname == any_quadrat)

## ----custom-title-point-shape--------------------------------------------
maps <- map_tag(bci_vft1_rnm, 
  site_name = "BCI 2012", point_size = 3, point_shape = c(17, 6), tag_size = 5
)
maps[1]

## ----custom-header-------------------------------------------------------
map_tag(bci_vft1_rnm, site_name = "BCI 2012", header = "My header")[1]

## ----header-basic--------------------------------------------------------
map_tag(bci_vft1_rnm, site_name = "BCI 2012", 
  header = "Line 1: _________\nLine 2:\nLine 3:....................."
)[1]

## ----header-via-helper---------------------------------------------------
your_header <- get_header(
  line1 = "Your header-line 1: _____________________________",
  line2 = "Your header-line 3: _____________________________",
  line3 = "Your header-line 2: _____________________________"
)
map_tag(bci_vft1_rnm, site_name = "BCI 2012", header = your_header)[1]

## ----pre-made-theme------------------------------------------------------
# Allow using pre-made themes (e.g. ggplot2::theme_bw()) and building custom
# themes (with ggplot::theme()).
library(ggplot2)

map_tag(bci_vft1_rnm, site_name = "BCI 2012", theme = theme_gray())[1]

## ----custom-theme--------------------------------------------------------
# An extreeme example -- to show that themes are extreemely flexible
your_theme <- ggplot2::theme(
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
map_tag(bci_vft1_rnm, site_name = "BCI 2012", theme = your_theme)[1]

## ------------------------------------------------------------------------
map_tag(bci_vft1_rnm, site_name = "BCI 2012", extend_grid = 0.4)[4]

## ------------------------------------------------------------------------
qx_and_qy_variables <- select(bci_vft1_rnm, qx, qy)
lapply(qx_and_qy_variables, range)

## ------------------------------------------------------------------------
map_tag(bci_vft1_rnm, site_name = "BCI 2012", 
  x_q = 20, x_sq = 5,
  y_q = 20, y_sq = 5
)[1]

## ------------------------------------------------------------------------
map_tag(bci_vft1_rnm, 
  site_name = "BCI 2012", x_q = 10, x_sq = 2.5, 
  # if not extended, the lines surrounding the map won't plot
  extend_grid = 0.25
)[1]

## ------------------------------------------------------------------------
# Using 
map_tag(bci_vft1_rnm, 
  site_name = "BCI 2012", x_q = 100, x_sq = 25, 
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
  extend_grid = -1.75
)[1]

## ----smaller-------------------------------------------------------------
# Creating new data set with qx and qy ranging 0-100
smaller <- bci_vft1_rnm
n <- nrow(smaller)
smaller$qx <- sample(0:10, n, replace = TRUE)
smaller$qy <- sample(0:10, n, replace = TRUE)

map_tag(smaller, x_q = 10, x_sq = 2.5, extend_grid = 0.25)[1]

## ------------------------------------------------------------------------
with_subquadrat <- add_subquadrat(bci_vft1_rnm, x_q = 20, x_sq = 5)
select(
  with_subquadrat, 
  # reorder variables to show first what's new 
  subquadrat, qx, qy, everything()
)

## ------------------------------------------------------------------------
maps <- map_tag(bci_vft1_rnm, x_q = 20, x_sq = 5)
data_list <- purrr::map(maps, "data")
data_combined <- purrr::reduce(data_list, rbind)
select(
  data_combined,
  subquadrat, qx, qy, everything()
)

