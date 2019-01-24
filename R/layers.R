axis_limits <- function(p, xlim = NULL, ylim = NULL) {
  # If user doesn't provide limits, set limits based on entire dataset
  data <- p[["data"]]
  xlim <- best_lim(xlim, data$gx)
  ylim <- best_lim(ylim, data$gy)

  p +
    coord_fixed(xlim = xlim, ylim = ylim) +
    scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2], 20)) +
    scale_y_continuous(minor_breaks = seq(ylim[1], ylim[2], 20))
}

add_species <- function(p,
                        data = NULL,
                        fill = "sp",
                        shape = 21,
                        point_size = 3) {
  check_add_species(p = p, data = data)

  if (fill != "sp") {
    # `z` = NULL because base may have `z`, e.g.: aes(z = elevation)`
    p <- p +
      suppressWarnings(
        geom_point(
          data = data, aes(gx, gy, z = NULL),
          shape = shape, size = point_size, fill = fill
        )
      )
    return(p)
  } else {
    p +
      suppressWarnings(
        geom_point(
          data = data,
          aes(gx, gy, z = NULL, fill = sp),
          shape = shape, size = point_size
        )
      )
  }
}

add_elevation_contours <- function(p,
                                   contour_size = 1,
                                   low = "blue",
                                   high = "red",
                                   bins = NULL) {
  check_crucial_names(p[["data"]], "elev")

  p +
    stat_contour(
      aes(x = gx, y = gy, z = elev, colour = ..level..),
      size = contour_size, bins = bins
    ) +
    scale_colour_continuous(low = low, high = high)
}

add_elevation_labels <- function(p,
                                 label_size = 3,
                                 label_color = "grey",
                                 xyjust = 1,
                                 fontface = "italic") {
  check_add_elevation_labels(
    p = p,
    label_color = label_color,
    label_size = label_size,
    xyjust = xyjust,
    fontface = fontface
  )

  p +
    text_at_max(
      max_elev(p)$x,
      label_size = label_size,
      label_color = label_color,
      xyjust = xyjust,
      fontface = fontface
    ) +
    text_at_max(
      max_elev(p)$y,
      label_size = label_size,
      label_color = label_color,
      xyjust = xyjust * 1.3,
      fontface = fontface
    )
}

max_elev <- function(p) {
  built <- ggplot_build(p)$data[[1]]
  elev <- mutate(built, gx = .data$x, gy = .data$y)
  elev_x <- elev[elev$gx == maximum(elev$gx), ]
  elev_y <- elev[elev$gy == maximum(elev$gy), ]
  list(x = elev_x, y = elev_y)
}

text_at_max <- function(x,
                        xyjust,
                        label_size = 3,
                        label_color = "grey",
                        fontface = "italic") {
  # Mute warning that `z` NULL
  suppressWarnings(
    geom_text(
      data = x,
      aes(label = level, z = NULL),
      size = label_size,
      color = label_color,
      hjust = xyjust,
      vjust = xyjust,
      fontface = fontface
    )
  )
}

hide_axis_labels <- function(p) {
  p + labs(x = NULL, y = NULL)
}

hide_color_legend <- function(p) {
  p + guides(color = "none")
}

hide_fill_legend <- function(p) {
  p + guides(fill = "none")
}

facet <- function(p, facets, ...) {
  p + facet_wrap(facets, ...)
}
