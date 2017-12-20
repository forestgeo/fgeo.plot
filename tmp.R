# map_quad() --------------------------------------------------------------

p <- map_quad(four_quadrats, extend_grid = 0)[[1]]
class(p)
class(p[[1]])


# map_tag() ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(map)
library(bciex)

# Subset of a public ViewFullTable from BCI (source:
# https://repository.si.edu/handle/10088/20925).

# Improve printing method
vft <- as_tibble(bci12vft_mini)
vft

# Filter the plot you want to map
vft1 <- dplyr::filter(vft, PlotID == 1)


# This data set has two wrong names that need to be fixed before using map_tag()
vft1_rnm <- dplyr::rename(vft1, qx = x, qy = y)
maps <- map_tag(vft1_rnm)

# Plotting only one map to screen
maps[1]



