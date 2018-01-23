# mapper ------------------------------------------------------------------

mapply_sp <- function(census,
  species,
  size = 3,
  shape = 21,
  wrap = TRUE,
  fill = "black",
  xlim = NULL,
  ylim = NULL,
  custom_theme = NULL,
  
  # elevation = NULL,
  # line_size = 0.5,
  # low = "#132B43",
  # high = "#56B1F7",
  # bins = NULL,
  ...) {
  check_map_sp(census = census, species = species, xlim = xlim, ylim = ylim)
  
  xlim <- pick_lim(xlim, census$gx)
  ylim <- pick_lim(ylim, census$gy)
  
  # Focus on given species
  cns <- census[census$sp %in% species, ]
  cns_list <- split(cns, cns$sp)
  p <- purrr::map2(
    .x = cns_list, .y = cns_list, map_sp2,
    size =  size,
    shape = shape,
    wrap = wrap,
    fill = fill,
    xlim = xlim,
    ylim = ylim,
    custom_theme = custom_theme,
    
    # elevation = elevation,
    # line_size = line_size,
    # low = low,
    # high = high,
    # bins = bins,
    ...
  )
  setNames(p, sort(species))
}

# Wrappers ----------------------------------------------------------------

map_elev <- function(elevation,
                     contour_size = 0.5,
                     low = "blue",
                     high = "red",
                     label_size = 3,
                     color = "grey",
                     fontface = "italic",
                     xlim = NULL,
                     ylim = NULL,
                     custom_theme = NULL,
                     ...) {
  base <- limit_gx_gy(map_gx_gy_elev(elevation), xlim = xlim, ylim = ylim)
  if (is.null(custom_theme)) {
    base <- theme_default(base)
  } else {
    base <- base + custom_theme
  }
  
  contour_elev(base, size = contour_size, ...) %>% 
    label_elev(size = label_size, color = color, fontface = fontface)
}

map_sp2 <- function(census, 
                    size = 3,
                    shape = 21,
                    wrap = TRUE,
                    fill = "sp",
                    xlim = NULL,
                    ylim = NULL,
                    custom_theme = NULL,
                    ...) {
  base <- limit_gx_gy(map_gx_gy(census), xlim = xlim, ylim = ylim)
  if (is.null(custom_theme)) {
    base <- theme_default(base)
  } else {
    base <- base + custom_theme
  }
  
  p <- add_sp(base, census, fill = fill, shape = shape, size = size)
  if (!wrap) {
    return(p)
  } else {
    facet_wrap_sp(p)
  }
}

# Base maps ---------------------------------------------------------------

#' @export
map_gx_gy_elev <- function(data) {
  ggplot(data, aes(gx, gy, z = elev))
}

#' @export
map_gx_gy <- function(data) {
  ggplot(data, aes(gx, gy))
}

# Limits ------------------------------------------------------------------

#' @export
limit_gx_gy <- function(p, xlim = NULL, ylim = NULL) {
  # If user doesn't provide limits, set limits based on entire dataset
  data <- p[["data"]]
  xlim <- pick_lim(xlim, data$gx)
  ylim <- pick_lim(ylim, data$gy)
  
  p +
    coord_fixed(xlim = xlim, ylim = ylim) +
    scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2], 20)) +
    scale_y_continuous(minor_breaks = seq(ylim[1], ylim[2], 20))
}

pick_lim <- function(lim, coord) {
  if (!is.null(lim)) {
    stopifnot(length(lim) == 2)
    return(lim)
  } else {
    lim <- c(0, max0(coord))
    lim
  }
}

# Layers ------------------------------------------------------------------

#' @export
add_sp <- function(p, data = NULL, fill = "sp", shape = 21, size = 3) {
  if (fill != "sp") {
    # `z` = NULL because base may have `z`, e.g.: aes(z = elevation)`
    p <- p + 
      suppressWarnings(
        geom_point(
          data = data, aes(gx, gy, z = NULL), 
          shape = shape, size = size, fill = fill
        )
      )
    return(p)
  } else {
   p + 
      suppressWarnings(
        geom_point(
          data = data, 
          aes(gx, gy, z = NULL, fill = sp), 
          shape = shape, size = size
        )
      )
    
  }
}

# Show params
# bins
# low, high
#' @export
contour_elev <- function(p, size = 1, low = "blue", high = "red", ...) {
   p +
    stat_contour(
      aes(x = gx, y = gy, z = elev, colour = ..level..), size = size, ...
    ) +
    scale_colour_continuous(low = low, high = high)
}

# Show params
# size
# color
# fontface
#' @export
label_elev <- function(p, xyjust = 1, ...) {
  p + 
    text_at_max(max_elev(p)$x, xyjust = xyjust, ...) + 
    text_at_max(max_elev(p)$y, xyjust = xyjust + 0.5, ...)
}

max_elev <- function(p) {
  built <- ggplot_build(p)$data[[1]]
  elev <-  mutate(built, gx = .data$x, gy = .data$y)
  elev_x <- elev[elev$gx == max0(elev$gx), ]
  elev_y <- elev[elev$gy == max0(elev$gy), ]
  list(x = elev_x, y = elev_y)
}

text_at_max <- function(x, xyjust, ...) {
  # Mute warning that `z` NULL
  suppressWarnings(
    geom_text(
      data = x, aes(label = level, z = NULL), 
      hjust = xyjust, vjust = xyjust,
      ...
    )
  )
}

# Labs --------------------------------------------------------------------

#' @export
hide_axis_labels <- function(p) {
  p + labs(x = NULL, y = NULL)
}

# Facets ------------------------------------------------------------------

#' Facets to add on top of ggplots based on ForestGEO's data.
#' 
#' @param ... Arguments passed to [ggplot2::facet_wrap()] and 
#'   [ggplot2::facet_grid()].
#' @seealso [ggplot2::facet_wrap()], [ggplot2::facet_grid()].
#' @name fgeo_facets
NULL

#' @rdname fgeo_facets
#' @export
facet_wrap_sp <- function(p, ...) {
  p + facet_wrap(~sp, ...)
}

#' @rdname fgeo_facets
#' @export
facet_h_sp <- function(p, ...) {
  p + facet_grid(.~sp, ...)
}

#' @rdname fgeo_facets
#' @export
facet_v_sp <- function(p, ...) {
  p + facet_grid(sp~., ...)
}

# Theme -------------------------------------------------------------------

#' @rdname themes
#' @export
theme_default <- function(p, 
                          panel.grid.minor = element_line(linetype = "dashed"),
                          ...) {
  p + 
    theme_bw() + 
    theme(panel.grid.minor = panel.grid.minor, ...)
}
