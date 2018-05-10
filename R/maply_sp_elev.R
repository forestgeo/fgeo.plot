#' Map species and elevation data.
#' 
#' These functions wrap a number of map elements for convenience:
#' * Use `map_sp_elev()` to map species and optionally elevation data in a 
#'   single page. You can map multiple species on the same plot or you can facet
#'   the output to map each species on a single plot and all plots in a single
#'   page.
#' * Use `map_elev()` if you want to map only elevation in the sipmlest way.
#' * Use `maply_sp_elev()` to apply the function `map_sp_elev()` to each species
#' in a census dataset. The output is not a map but a list of maps, one per
#' species, that can be printed on a .pdf file.
#' 
#' @template compare_ggplot2
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
#' @param hide_legend_color Logical; `TRUE` hides the color legend.
#' @inheritParams label_elev
#' @inheritParams limit_gx_gy
#' @param custom_theme A valed [ggplot2::theme()]. `NULL` uses the default
#'   theme [theme_default()].
#'
#' @seealso [map_gx_gy_elev()], [limit_gx_gy()], [add_sp()], [contour_elev()],
#'   [label_elev()], [hide()], [wrap()]
#' @family `maply_*` functions.
#' 
#' @return 
#' * `maply_sp_elev()` returns a list of ggplots.
#' * `map_elev()` and `map_sp_elev()` return a ggoplot.
#' 
#' @export 
#' @examples
#' census <- fgeo.tool::row_top(bciex::bci12s7mini, sp, 2)
#' elevation <- bciex::bci_elevation
#' 
#' # Map on multiple pages ---------------------------------------------------
#' 
#' p1 <- maply_sp_elev(census)
#' # Showing first map only.
#' p1[[1]]
#' 
#' p2 <- maply_sp_elev(census, elevation)
#' # Showing second map only.
#' p2[[2]]
#' 
#' # Tweaking
#' p3 <- maply_sp_elev(
#'   census,
#'   elevation,
#'   species = "all",
#'   fill = "white",
#'   shape = 21,
#'   point_size = 5,
#'   contour_size = 1,
#'   low = "grey",
#'   high = "black",
#'   hide_legend_color = TRUE,
#'   bins = 7,
#'   label_elev = FALSE
#' )
#' p3[[1]]
#' 
#' # Map on a single page (maybe multiple panels) ----------------------------
#' 
#' # Simplest way to map elevation data only
#' map_elev(elevation)
#' 
#' map_sp_elev(census)
#' 
#' map_sp_elev(census, elevation)
#' 
#' # For maximum control, you can compose maps as you like
#' 
#' # Traditional: g(f(x))
#' contour_elev(map_gx_gy_elev(elevation))
#' 
#' # With the pipe: f(x) %>% g()
#' map_gx_gy_elev(elevation) %>%
#'   contour_elev()
#'
#' # With traditional sintax: As you add more functions readability decreases.
#' theme_default(
#'   wrap(
#'     add_sp(
#'       hide_legend_color(
#'         hide_axis_labels(
#'           label_elev(
#'             contour_elev(
#'               limit_gx_gy(
#'                 map_gx_gy_elev(elevation), 
#'                 xlim = c(0, 1200)
#'               ),
#'               contour_size = 0.5
#'             ),
#'             label_color = "red"
#'           )
#'         )
#'       ), census, point_size = 5
#'     ), "sp"
#'   ), legend.position = "top"
#' )
#'
#' # Same with the pipe: As you add more functions readability doesn't change
#' map_gx_gy_elev(elevation) %>%
#'   limit_gx_gy(xlim = c(0, 1200)) %>%
#'   contour_elev(contour_size = 0.5) %>%
#'   label_elev(label_color = "red") %>%
#'   hide_axis_labels() %>%
#'   hide_legend_color() %>%
#'   add_sp(census, point_size = 5) %>%
#'   wrap("sp") %>%
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
                           hide_legend_color = FALSE,
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
    hide_legend_color = hide_legend_color,
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
                        hide_legend_color = FALSE,
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
  fgeo.base::check_crucial_names(census, c("gx", "gy"))
  
  
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
      hide_legend_color = hide_legend_color,
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
                     hide_legend_color = FALSE,
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
    hide_legend_color = hide_legend_color,
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
                          hide_legend_color = FALSE,
                          bins = NULL,
                          label_elev = TRUE,
                          label_size = 3,
                          label_color = "grey",
                          xyjust = 1,
                          fontface = "italic") {
  elevation <- fgeo.tool::restructure_elev(elevation)
  base <- elevation %>% 
    map_gx_gy_elev() %>% 
    contour_elev(
      contour_size = contour_size, low = low, high = high, bins = bins
    ) %>% 
    best_elev_legend(hide_legend_color = hide_legend_color)
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
#' @seealso [map_sp_elev()], [map_elev()], [maply_sp_elev()], 
#'   [ggplot2::ggplot()].
#' @family map components.
#' @export
#' @examples 
#' census <- fgeo.tool::row_top(bciex::bci12s7mini, sp, 2)
#' elevation <- bciex::bci_elevation
#' 
#' # Look alike but internally they are not. 
#' base_elev <- map_gx_gy_elev(elevation)
#' base_census <- map_gx_gy(census)
#' 
#' # Works
#' contour_elev(base_elev)
#' 
#' # # Fails.
#' # \dontrun{
#' # add_sp(base_elev)
#' # contour_elev(base_census)
#' # }
#' 
#' # Supply the correct base
#' add_sp(base_census)
#' contour_elev(base_elev)
map_gx_gy_elev <- function(data) {
  data <- fgeo.tool::restructure_elev(data)
  ggplot(data, aes(gx, gy, z = elev))
}

#' @rdname map_gx_gy_elev
#' @export
map_gx_gy <- function(data) {
  check_map_gx_gy(data)
  
  ggplot(data, aes(gx, gy))
}

# Limits ------------------------------------------------------------------

#' Set the map limits.
#' 
#' @template p
#' @template xlim_ylim
#' 
#' @seealso [map_sp_elev()], [maply_sp_elev()], [ggplot2::coord_fixed()].
#' 
#' @family map components.
#' @export
#' @examples 
#' census <- bciex::bci12s7mini
#' map <- map_gx_gy(census)
#' limit_gx_gy(map, xlim = c(0, 500), c(0, 750))
#' 
#' elevation <- bciex::bci_elevation
#' map <- map_gx_gy_elev(elevation)
#' limit_gx_gy(map, xlim = c(0, 500), c(0, 750))
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
#' @seealso [map_sp_elev()], [maply_sp_elev()], [ggplot2::geom_point()].
#' 
#' @family map components.
#' @export
#' @examples 
#' census <- fgeo.tool::row_top(bciex::bci12s7mini, sp, 2)
#' 
#' add_sp(map_gx_gy(census))
#' 
#' add_sp(map_gx_gy(census), fill = "white", shape = 22, point_size = 4)
add_sp <- function(p, data = NULL, fill = "sp", shape = 21, point_size = 3) {
  check_add_sp(p = p, data = data)
  
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
#' @seealso [maply_sp_elev()], [map_sp_elev()], [map_elev()], 
#'   [ggplot2::stat_contour()].
#' 
#' @family map components.
#' @export
#' @examples 
#' elevation <- bciex::bci_elevation
#' 
#' base_elev <- map_gx_gy_elev(elevation)
#' 
#' contour_elev(base_elev)
#' 
#' contour_elev(
#'   base_elev, contour_size = 0.5, low = "grey", high = "black", bins = 4
#' )
contour_elev <- function(p, 
                         contour_size = 1, 
                         low = "blue", 
                         high = "red", 
                         bins = NULL) {
  fgeo.base::check_crucial_names(p[["data"]], "elev")
  
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
#' @seealso [maply_sp_elev()], [map_sp_elev()], [map_elev()], 
#'   [ggplot2::geom_text()].
#' 
#' @family map components.
#' @export
#' @examples 
#' elevation <- bciex::bci_elevation
#' 
#' contour_elev(map_gx_gy_elev(elevation))
#' 
#' contour_elev(map_gx_gy_elev(elevation)) %>% 
#'   # Pusing elevation label out of the plot area and removig color legend
#'   limit_gx_gy(xlim = c(0, 1010), ylim = c(0, 510)) %>% 
#'   label_elev(
#'     label_size = 2, label_color = "black", xyjust = -0.25, fontface = "bold"
#'   ) %>% 
#'   hide_legend_color()
label_elev <- function(p, 
                       label_size = 3,
                       label_color = "grey",
                       xyjust = 1, 
                       fontface = "italic") {
  check_label_elev(
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



# Map components ==========================================================

# Hide --------------------------------------------------------------------

#' Hide elements of a plot.
#' 
#' `hide_axis_labels` hides axis labels.
#' `hide_legend_color` hides the color legend, for example, of elevation lines.
#' 
#' @template p
#' 
#' @seealso [ggplot2::labs()],  [ggplot2::guides()].
#' @family map components.
#' @examples 
#' elevation <- bciex::bci_elevation
#' map <- map_gx_gy_elev(elevation)
#' hide_axis_labels(map)
#' 
#' with_color_legend <- contour_elev(map_gx_gy_elev(elevation))
#' hide_legend_color(with_color_legend)
#' @name hide 
NULL

#' @rdname hide
#' @export
hide_axis_labels <- function(p) {
  p + labs(x = NULL, y = NULL)
}

#' @rdname hide
#' @export
hide_legend_color <- function(p) {
  p + guides(color = "none")
}

# Facets ------------------------------------------------------------------

# @param ... Arguments passed to [ggplot2::facet_wrap()] and 
#   [ggplot2::facet_grid()].



#' Facet data by a variable, and wrap multiple plots to fit a single page.
#' 
#' This function behaves like [ggplot2::facet_wrap()] but it is designed to 
#' allow using it as part of a pipe.
#' 
#' @template p
#' @inheritParams ggplot2::facet_wrap
#' @inheritDotParams ggplot2::facet_wrap
#' @seealso [ggplot2::facet_wrap()], [ggplot2::facet_grid()].
#' @family map components.
#' 
#' @export
#' @examples
#' census <- bciex::bci12s7mini
#' species <- c("hybapr", "faraoc")
#' two_sp <- dplyr::filter(census, sp %in% species)
#' 
#' add_sp(map_gx_gy(two_sp))
#' 
#' add_sp(map_gx_gy(two_sp)) %>% 
#'   wrap("sp")
#' # Same
#' add_sp(map_gx_gy(two_sp)) %>% 
#'   wrap(~sp)
#' 
#' add_sp(map_gx_gy(two_sp)) %>% 
#'   wrap("status")
#' 
#' add_sp(map_gx_gy(two_sp)) %>% 
#'   wrap(c("sp", "status"))
#' # Same
#' add_sp(map_gx_gy(two_sp)) %>% 
#'   wrap(~ sp + status)
wrap <- function(p, facets, ...) {
  p + facet_wrap(facets, ...)
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
    wrap(p, "sp")
  }
}

best_elev_legend <- function(p, hide_legend_color = FALSE) {
  if (!hide_legend_color) {
    return(p)
  } else {
    hide_legend_color(p)
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
  fgeo.base::check_crucial_names(census, c("gx", "gy", "sp"))
  if (length(species) == 0) {
    rlang::abort("`sp` must be not empty.")
  }
  invisible(census)
}

check_add_sp <- function(p, data) {
  p_data_laks_sp <- !any(grepl("sp", names(p[["data"]])))
  if (all(is.null(data), p_data_laks_sp)) {
    msg <- paste0(
      "The plot fed to `add_sp()` lacks the variable `sp`\n",
      "* Did you forget to provide census data?"
    )
    abort(msg)
  }
  invisible(p)
}

check_label_elev <- function(p, label_color, label_size, xyjust, fontface) {
  stopifnot(
    is.character(label_color), 
    is.numeric(label_size),
    is.numeric(xyjust),
    is.character(fontface) 
  )
  p_data <- ggplot_build(p)[["data"]][[1]]
  p_data_lacks_level <- !any(grepl("level", names(p_data)))
  if (p_data_lacks_level) {
    msg <- paste0(
      "The plot fed to `level_elev() lacks the variable `level`\n",
      "* Did you forget to call `contour_elev()` before `level_elev`?"
    )
    abort(msg)
  }
  invisible(p)
}

check_map_gx_gy <- function(data) {
  if (rlang::has_name(data, "elev")) {
    msg <- paste0(
      "This function is designed not for elevation but for census data.\n",
      "* If you plan to map elevation lines, use instead `map_gx_gy_elev()`."
    )
    warn(msg)
  }
  invisible(data)
}
