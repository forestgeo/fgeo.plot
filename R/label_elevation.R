# # Examples
# library(tidyverse)
# library(fgeo)
# 
# census <- bciex::bci12t7mini
# elevation <- rename(bciex::bci_elevation, gx = x, gy = y)
# 
# w_elev <- map_elevation(elevation)
# w_elev
# 
# label_elevation(w_elev)

map_elevation <- function(data, color = "black", theme = theme_map_sp(), ...) {
  fgeo.tool::check_crucial_names(data, c("gx", "gy", "elev"))
  w_elev <- ggplot(data = data, aes(gx, gy, z = elev)) +
    geom_contour(color = color, ...) +
    theme
}

