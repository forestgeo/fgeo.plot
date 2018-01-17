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

label_elevation <- function(w_elev,
                            size = 3,
                            hjust = -0.2,
                            fontface = "italic",
                            color = "grey",
                            ...) {
  built <- ggplot_build(w_elev)$data[[1]]
  elev <-  mutate(built, gx = x, gy = y)
  elev_x <- elev[elev$gx == max0(elev$gx), ]
  elev_y <- elev[elev$gy == max0(elev$gy), ]
  w_elev + 
    text_at_max(
      elev_x, 
      size = size, 
      hjust = hjust, 
      fontface = fontface, 
      color = color,
      ...
    ) + 
    text_at_max(
      elev_y, 
      size = size, 
      hjust = hjust, 
      fontface = fontface, 
      color = color,
      ...
    )
}

text_at_max <- function(x, ...) {
  suppressWarnings({geom_text(data = x, aes(label = level, z = NULL), ...)})
}
