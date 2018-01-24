# Mapper ------------------------------------------------------------------

mapply_sp_elev <- function(census,
                           elevation = NULL,
                           species = "all",
                           fill = "black",
                           shape = 21,
                           wrap = TRUE,
                           point_size = 3,
                           contour_size = 0.5,
                           low = "blue",
                           high = "red",
                           hide_legend_elev = FALSE,
                           bins = NULL,
                           label_elev = TRUE,
                           label_size = 3,
                           label_color = "grey",
                           xyjust = 1,
                           fontface = "italic",
                           xlim = NULL,
                           ylim = NULL,
                           custom_theme = NULL) {
  # TODO: rename to check_mapply_sp_elev
  check_map_sp(census = census, species = species, xlim = xlim, ylim = ylim)
  
  species <- best_species(census, species)
  
  xlim <- best_lim(xlim, census$gx)
  ylim <- best_lim(ylim, census$gy)
  
  # Focus on given species
  cns <- census[census$sp %in% species, ]
  cns_list <- split(cns, cns$sp)
  p <- lapply(
    X = cns_list, FUN = map_sp_elev,
    elevation = elevation,
    fill = fill,
    shape = shape,
    wrap = wrap,
    point_size = point_size,
    contour_size = contour_size,
    low = low,
    high = high,
    hide_legend_elev = hide_legend_elev,
    bins = bins,
    label_elev = label_elev,
    label_size = label_size,
    label_color = label_color,
    xyjust = xyjust,
    fontface = fontface,
    xlim = xlim,
    ylim = ylim,
    custom_theme = custom_theme
  )
  setNames(p, species)
}

# Wrappers ----------------------------------------------------------------

map_sp_elev <- function(census,
                        elevation = NULL,
                        fill = "black",
                        shape = 21,
                        wrap = TRUE,
                        point_size = 3,
                        contour_size = 0.5,
                        low = "blue",
                        high = "red",
                        hide_legend_elev = FALSE,
                        bins = NULL,
                        label_elev = TRUE,
                        label_size = 3,
                        label_color = "grey",
                        xyjust = 1,
                        fontface = "italic",
                        xlim = NULL,
                        ylim = NULL,
                        custom_theme = NULL) {
  stopifnot(!is.null(census))
  
  # User doesn't provide elevation data
  if (is.null(elevation)) {
    base <- map_gx_gy(census)
    
    # User provides elevation data
  } else {
    base <- map_pure_elev(
      elevation = elevation,
      contour_size = contour_size,
      low = low,
      high = high,
      hide_legend_elev = hide_legend_elev,
      bins = bins,
      label_elev = label_elev,
      label_size = label_size,
      label_color = label_color,
      xyjust = xyjust,
      fontface = fontface
    )
  }
  
  base %>% 
    add_sp(census, fill = fill, shape = shape, point_size = point_size) %>%
    best_layout(wrap = wrap) %>% 
    limit_gx_gy(xlim = xlim, ylim = ylim) %>%
    best_theme(custom_theme = custom_theme)
}

map_pure_elev <- function(elevation,
                          contour_size = 0.5,
                          low = "blue",
                          high = "red",
                          hide_legend_elev = FALSE,
                          bins = NULL,
                          label_elev = TRUE,
                          label_size = 3,
                          label_color = "grey",
                          xyjust = 1,
                          fontface = "italic") {
  base <- elevation %>% 
    map_gx_gy_elev() %>% 
    contour_elev(
      contour_size = contour_size, low = low, high = high, bins = bins
    ) %>% 
    best_elev_legend(hide_legend_elev = hide_legend_elev)
  if (label_elev) {
    base <- label_elev(
      base,
      label_size = label_size,
      label_color = label_color,
      xyjust = xyjust,
      fontface = fontface
    )
  }
  base
}

map_elev <- function(elevation,
                     contour_size = 0.5,
                     low = "blue",
                     high = "red",
                     hide_legend_elev = FALSE,
                     bins = NULL,
                     label_elev = TRUE,
                     label_size = 3,
                     label_color = "grey",
                     xyjust = 1,
                     fontface = "italic",
                     xlim = NULL,
                     ylim = NULL,
                     custom_theme = NULL) {
  base <- map_pure_elev(
    elevation = elevation,
    contour_size = contour_size,
    low = low,
    high = high,
    hide_legend_elev = hide_legend_elev,
    bins = bins,
    label_elev = label_elev,
    label_size = label_size,
    label_color = label_color,
    xyjust = xyjust,
    fontface = fontface
  )
  base %>% 
    limit_gx_gy(xlim = xlim, ylim = ylim) %>%
    best_theme(custom_theme = custom_theme)
}

# Simplify conditions -----------------------------------------------------

best_species <- function(census, species) {
  if (!identical(species, "all")) {
    return(sort(species))
  } else {
    sort(unique(census$sp))
  }
}

best_theme <- function(p, custom_theme) {
  if (is.null(custom_theme)) {
    return(theme_default(p))
  } else {
    p + custom_theme
  }
}

best_layout <- function(p, wrap = FALSE) {
  if (!wrap) {
    return(p)
  } else {
    facet_wrap_sp(p)
  }
}

best_elev_legend <- function(p, hide_legend_elev = FALSE) {
  if (!hide_legend_elev) {
    return(p)
  } else {
    hide_legend_elev(p)
  }
}

best_lim <- function(lim, coord) {
  if (!is.null(lim)) {
    stopifnot(length(lim) == 2)
    return(lim)
  } else {
    lim <- c(0, max0(coord))
    lim
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
  xlim <- best_lim(xlim, data$gx)
  ylim <- best_lim(ylim, data$gy)
  
  p +
    coord_fixed(xlim = xlim, ylim = ylim) +
    scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2], 20)) +
    scale_y_continuous(minor_breaks = seq(ylim[1], ylim[2], 20))
}

# Layers ------------------------------------------------------------------

#' @export
add_sp <- function(p, data = NULL, fill = "sp", shape = 21, point_size = 3) {
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

# Show params
# bins
# low, high
#' @export
contour_elev <- function(p, 
                         contour_size = 1, 
                         low = "blue", 
                         high = "red", 
                         bins = bins) {
   p +
    stat_contour(
      aes(x = gx, y = gy, z = elev, colour = ..level..), 
      size = contour_size, bins = bins
    ) +
    scale_colour_continuous(low = low, high = high)
}

# Show params
# size
# color
# fontface
#' @export
label_elev <- function(p, 
                       label_size = 3,
                       label_color = "grey",
                       xyjust = 1, 
                       fontface = "italic") {
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
      xyjust = xyjust + 0.5,
      fontface = fontface
    )
}

max_elev <- function(p) {
  built <- ggplot_build(p)$data[[1]]
  elev <-  mutate(built, gx = .data$x, gy = .data$y)
  elev_x <- elev[elev$gx == max0(elev$gx), ]
  elev_y <- elev[elev$gy == max0(elev$gy), ]
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
      color =  label_color,
      hjust = xyjust,
      vjust = xyjust,
      fontface =  fontface
    )
  )
}

# Labs --------------------------------------------------------------------

#' @export
hide_axis_labels <- function(p) {
  p + labs(x = NULL, y = NULL)
}

hide_legend_elev <- function(p) {
  p + guides(color = "none")
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
