# Wrappers ----------------------------------------------------------------

#' Wrappers to map species and elevation data.
#' 
#' These functions wrap a number of map elements for convenience:
#' * `maply_sp_elev()` applies the function `map_sp_elev()` to each species in
#' the census dataset taht you provide. It outputs a list of maps, one per
#' species, that can be printed on a .pdf file.
#' * `map_sp_elev()` maps species and optionally elevation data. 
#' * `map_elev()` is a smaller, simpler, wrapper to map only elevation data.
#' 
#' @param census A dataframe; specifically, a ForestGEO's census.
#' @param elevation A list or dataframe giving ForestGEO's elevation-data.
#' @param species A character vector. Each element of the vector must be the 
#'   code for one species in the column `sp`. This function will produce as 
#'   many maps as elements in this vector. The string "all" is a shortcut to 
#'   map all unique codes in the column `sp`.
#' @inheritParams add_sp
#' @param wrap (Not available for `maply_sp_elev()`) Logical; `TRUE` wraps
#'   multiple maps within the area of a single graphic plot.
#' @param label_elev Logical. `TRUE` labels the elevation lines with text.
#' @inheritParams contour_elev
#' @param hide_legend_elev Logical; `TRUE` hides the color legend.
#' @inheritParams label_elev
#' @inheritParams limit_gx_gy
#' @param custom_theme A valed [ggplot2::theme()]. `NULL` uses the default
#'   theme [theme_default()].
#'
#' @return 
#' * `maply_sp_elev()` returns a list of ggplots
#' * `map_elev()` and `map_sp_elev()` return a ggoplot.
#' 
#' @seealso map_gx_gy_elev
#' @family map wrappers.
#' @export 
#'
#' @examples
#' census <- fgeo.tool::top(bciex::bci12s7mini, sp, 2)
#' 
#' elevation <- bciex::bci_elevation
#' head(elevation)
#' elevation <- fgeo.tool::restructure_elev(bciex::bci_elevation)
#' head(elevation)
#' 
#' # Showing first plot only.
#' p <- maply_sp_elev(census)
#' p[[1]]
#' 
#' 
#' p <- maply_sp_elev(
#'   census,
#'   elevation,
#'   species = "all",
#'   fill = "white",
#'   shape = 21,
#'   point_size = 5,
#'   contour_size = 1,
#'   low = "grey",
#'   high = "black",
#'   hide_legend_elev = TRUE,
#'   bins = 7,
#'   label_elev = FALSE
#' )
#' p[[1]]
#' 
#' # Same but outputs a plot, not a list of plots
#' map_sp_elev(census, elevation)
#' 
#' # Similar but maps elevation exclusively
#' map_elev(elevation)
#' 
#' # For maximum control, you can compose maps as you like
#' map_gx_gy_elev(elevation) %>%
#'   limit_gx_gy(xlim = c(0, 1200)) %>%
#'   contour_elev(contour_size = 0.5) %>%
#'   label_elev(label_color = "red") %>%
#'   hide_axis_labels() %>%
#'   hide_legend_elev() %>%
#'   add_sp(census, point_size = 5) %>%
#'   facet_h_sp() %>%
#'   theme_default(legend.position = "top")
maply_sp_elev <- function(census,
                           elevation = NULL,
                           species = "all",
                           fill = "black",
                           shape = 21,
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
  check_sp(census = census, species = species)

  species <- best_species(census, species)
  xlim <- best_lim(xlim, census$gx)
  ylim <- best_lim(ylim, census$gy)
  
  # Iterate over each of the given species
  cns <- census[census$sp %in% species, ]
  cns_list <- split(cns, cns$sp)
  p <- lapply(
    X = cns_list, FUN = map_sp_elev,
    elevation = elevation,
    fill = fill,
    shape = shape,
    wrap = TRUE,
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

#' @rdname maply_sp_elev
#' @export
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
  stopifnot(is.data.frame(census))
  fgeo.tool::check_crucial_names(census, c("gx", "gy"))
  
  
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

#' @rdname maply_sp_elev
#' @export
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
    fgeo.tool::restructure_elev() %>% 
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

# Base maps ---------------------------------------------------------------

#' Map a base over which other map components can later be added.
#' 
#' @template data_ggplot
#' 
#' @name map_gx_gy_elev
#' @family map components.
NULL

#' @export
#' @rdname map_gx_gy_elev
map_gx_gy_elev <- function(data) {
  ggplot(data, aes(gx, gy, z = elev))
}

#' @rdname map_gx_gy_elev
#' @export
map_gx_gy <- function(data) {
  ggplot(data, aes(gx, gy))
}

# Limits ------------------------------------------------------------------

#' Set the map limits.
#' 
#' @template p
#' @template xlim_ylim
#' 
#' @family map components.
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

#' Represent species with points.
#' 
#' @template p
#' @template data_ggplot
#' @param fill Character; either a colour or "sp", which maps each species to a
#'   different color.
#' @template shape_point_size
#' 
#' @family map components.
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

#' Represent elevation with lines.
#' 
#' @template p
#' @param contour_size A number giving the size of the contour of elevation
#'   lines. Passed to [ggplot2::stat_contour()].
#' @template low_high
#' @param bins A number giving the number of elevation lines to map.
#' 
#' @family map components.
#' @export
contour_elev <- function(p, 
                         contour_size = 1, 
                         low = "blue", 
                         high = "red", 
                         bins = NULL) {
   p +
    stat_contour(
      aes(x = gx, y = gy, z = elev, colour = ..level..), 
      size = contour_size, bins = bins
    ) +
    scale_colour_continuous(low = low, high = high)
}

#' Label elevation lines.
#' 
#' @template p
#' @template label_size_label_color_fontface
#' @param xyjust A number to adjust the position of the text labels of the 
#'   elevation lines.
#' 
#' @family map components.
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
      xyjust = xyjust * 1.3,
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

#' Hide axis labels.
#' 
#' @template p
#' @family map components.
#' @export
hide_axis_labels <- function(p) {
  p + labs(x = NULL, y = NULL)
}

#' Hide the color legend of elevation lines.
#' 
#' @template p
#' @family map components.
#' @export
hide_legend_elev <- function(p) {
  p + guides(color = "none")
}

# Facets ------------------------------------------------------------------

#' Facet maps vertically, hirizontally, or wrap them to fit a page.
#' 
#' @template p
#' @param ... Arguments passed to [ggplot2::facet_wrap()] and 
#'   [ggplot2::facet_grid()].
#' @seealso [ggplot2::facet_wrap()], [ggplot2::facet_grid()].
#' @family map components.
#' 
#' @name facets
NULL

#' @rdname facets
#' @export
facet_wrap_sp <- function(p, ...) {
  p + facet_wrap(~sp, ...)
}

#' @rdname facets
#' @export
facet_h_sp <- function(p, ...) {
  p + facet_grid(.~sp, ...)
}

#' @rdname facets
#' @export
facet_v_sp <- function(p, ...) {
  p + facet_grid(sp~., ...)
}



# UTILS ===================================================================

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
    if (length(lim) != 2) {
      abort("Limits must be in a numeric vector of length 2; e.g. `c(0, 500)`.")
    }
    return(lim)
  } else {
    lim <- c(0, max0(coord))
    lim
  }
}

# Checks ------------------------------------------------------------------

check_sp <- function(census, species) {
  stopifnot(is.data.frame(census))
  stopifnot(is.character(species))
  fgeo.tool::check_crucial_names(census, c("gx", "gy", "sp"))
  if (length(species) == 0) {
    rlang::abort("`sp` must be not empty.")
  }
  invisible(census)
}
