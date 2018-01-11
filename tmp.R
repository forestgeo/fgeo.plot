










point_fill_sp <- function(shape = 21, ...){
  geom_point(aes(gx, gy, fill = sp), shape = shape, ...)
}

contour_colour_range <- function(low, high) {
  scale_colour_continuous(low = low, high = high)
}

map_sp_each <- function(cns,
                        xlim,
                        ylim,
                        theme,
                        elevation,
                        line_size,
                        low,
                        high,
                        bins,
                        ...) {
  # Species
  p <- map_base(x = cns, xlim = xlim, ylim = ylim, theme = theme) +
    geom_point(...) +
    labs(title = unique(cns$sp))

  # If elevation is provided
  if (!is.null(elevation)) {
    p <- add_elev(
      p,
      elevation = elevation,
      line_size = line_size,
      bins = bins,
      high = high,
      low = low
    )
  }

  # Species, or species + elevation
  p
}


