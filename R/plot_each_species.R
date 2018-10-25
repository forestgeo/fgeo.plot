#' Map species and elevation data.
#' 
#' These functions wrap a number of map elements for convenience:
#' * Use `plot_species_or_elevation()` to map species and optionally elevation
#' data in a single page. You can map multiple species on the same plot or you
#' can facet the output to map each species on a single plot and all plots in a
#' single page.
#' * Use `plot_elevation()` if you want to map only elevation in the simplest
#' way.
#' * Use `plot_each_species()` to apply the function
#' `plot_species_or_elevation()` to each species in a census dataset. The output
#' is not a map but a list of maps, one per species, that can be printed on a
#' .pdf file.
#' 
#' @template compare_ggplot2
#' 
#' @param census A dataframe; specifically, a ForestGEO's census.
#' @param elevation A list or dataframe giving ForestGEO's elevation-data.
#' @param species A character vector. Each element of the vector must be the 
#'   code for one species in the column `sp`. This function will produce as 
#'   many maps as elements in this vector. The string "all" is a shortcut to 
#'   map all unique codes in the column `sp`.
#' @inheritParams add_species
#' @param add_elevation_labels Logical. `TRUE` labels the elevation lines with
#'   text.
#' @inheritParams add_elevation_contrours
#' @param hide_color_legend Logical; `TRUE` hides the color legend.
#' @inheritParams add_elevation_labels
#' @inheritParams axis_limits
#' @param custom_theme A valid [ggplot2::theme()]. `NULL` uses the default
#'   theme [theme_default()].
#'
#' @seealso [plot_base_elevation()], [axis_limits()], [add_species()],
#'   [add_elevation_contrours()], [add_elevation_labels()], [hide()], [facet()].
#'
#' @family functions to create a list of plots
#' 
#' @return 
#' * `plot_each_species()` returns a list of ggplots.
#' * `plot_elevation()` and `plot_species_or_elevation()` return a ggplot.
#' 
#' @export 
#' @examples
#' # Small dataset with a few species for quick examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
#' elevation <- fgeo.data::luquillo_elevation
#' 
#' p1 <- plot_each_species(census)
#' # Showing first map only.
#' p1[[1]]
#' 
#' p2 <- plot_each_species(census, elevation)
#' # Showing second map only.
#' p2[[2]]
#' 
#' # Tweaking
#' p3 <- plot_each_species(
#'   census,
#'   elevation,
#'   species = "all",
#'   fill = "white",
#'   shape = 21,
#'   point_size = 5,
#'   contour_size = 1,
#'   low = "grey",
#'   high = "black",
#'   hide_color_legend = TRUE,
#'   bins = 7,
#'   add_elevation_labels = FALSE
#' )
#' p3[[1]]
plot_each_species <- function(census,
                              elevation = NULL,
                              species = "all",
                              fill = "black",
                              shape = 21,
                              point_size = 3,
                              contour_size = 0.5,
                              low = "blue",
                              high = "red",
                              hide_color_legend = FALSE,
                              bins = NULL,
                              add_elevation_labels = TRUE,
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
    X = cns_list, FUN = plot_species_or_elevation,
    elevation = elevation,
    fill = fill,
    shape = shape,
    facet = TRUE,
    point_size = point_size,
    contour_size = contour_size,
    low = low,
    high = high,
    hide_color_legend = hide_color_legend,
    bins = bins,
    add_elevation_labels = add_elevation_labels,
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

#' @inherit plot_each_species title description details return
#' @inheritParams plot_each_species
#' @param facet (Not available for `plot_each_species()`) Logical; `TRUE` wraps
#'   multiple maps within the area of a single graphic plot.
#' 
#' @family functions to create a single plot
#' 
#' @export
#' @examples 
#' # Small dataset with a few species for quick examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
#' elevation <- fgeo.data::luquillo_elevation
#' 
#' # Simplest way to map elevation data only
#' plot_elevation(elevation)
#' 
#' plot_species_or_elevation(census)
#' 
#' plot_species_or_elevation(census, elevation)
#' 
#' # For maximum control, you can compose maps as you like
#' # Traditional sintax: g(f(x))
#' add_elevation_contrours(plot_base_elevation(elevation))
#' 
#' # With the pipe: f(x) %>% g()
#' plot_base_elevation(elevation) %>%
#'   add_elevation_contrours()
#' 
#' # The traditional sintax is hard to read when you compose multiple functions.
#' # With the pipe, readability isn't affected by the number of functions.
#' plot_base_elevation(elevation) %>%
#'   axis_limits(xlim = c(-100, 400)) %>%
#'   add_elevation_contrours(contour_size = 0.5) %>%
#'   add_elevation_labels(label_color = "red") %>%
#'   hide_axis_labels() %>%
#'   hide_color_legend() %>%
#'   add_species(census, point_size = 5) %>%
#'   facet("sp") %>%
#'   theme_default(legend.position = "top")
plot_species_or_elevation <- function(census,
                                      elevation = NULL,
                                      fill = "black",
                                      shape = 21,
                                      facet = TRUE,
                                      point_size = 3,
                                      contour_size = 0.5,
                                      low = "blue",
                                      high = "red",
                                      hide_color_legend = FALSE,
                                      bins = NULL,
                                      add_elevation_labels = TRUE,
                                      label_size = 3,
                                      label_color = "grey",
                                      xyjust = 1,
                                      fontface = "italic",
                                      xlim = NULL,
                                      ylim = NULL,
                                      custom_theme = NULL) {
  stopifnot(is.data.frame(census))
  check_crucial_names(census, c("gx", "gy"))
  
  # User doesn't provide elevation data
  if (is.null(elevation)) {
    base <- plot_base_census(census)
    
    # User provides elevation data
  } else {
    base <- map_pure_elev(
      elevation = elevation,
      contour_size = contour_size,
      low = low,
      high = high,
      hide_color_legend = hide_color_legend,
      bins = bins,
      add_elevation_labels = add_elevation_labels,
      label_size = label_size,
      label_color = label_color,
      xyjust = xyjust,
      fontface = fontface
    )
  }
  
  base %>% 
    add_species(census, fill = fill, shape = shape, point_size = point_size) %>%
    best_layout(facet = facet) %>% 
    axis_limits(xlim = xlim, ylim = ylim) %>%
    best_theme(custom_theme = custom_theme)
}




#' @rdname plot_species_or_elevation
#' @export
plot_elevation <- function(elevation,
                           contour_size = 0.5,
                           low = "blue",
                           high = "red",
                           hide_color_legend = FALSE,
                           bins = NULL,
                           add_elevation_labels = TRUE,
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
    hide_color_legend = hide_color_legend,
    bins = bins,
    add_elevation_labels = add_elevation_labels,
    label_size = label_size,
    label_color = label_color,
    xyjust = xyjust,
    fontface = fontface
  )
  base %>% 
    axis_limits(xlim = xlim, ylim = ylim) %>%
    best_theme(custom_theme = custom_theme)
}

map_pure_elev <- function(elevation,
                          contour_size = 0.5,
                          low = "blue",
                          high = "red",
                          hide_color_legend = FALSE,
                          bins = NULL,
                          add_elevation_labels = TRUE,
                          label_size = 3,
                          label_color = "grey",
                          xyjust = 1,
                          fontface = "italic") {
  elevation <- fgeo.tool::fgeo_elevation(elevation)
  base <- elevation %>% 
    plot_base_elevation() %>% 
    add_elevation_contrours(
      contour_size = contour_size, low = low, high = high, bins = bins
    ) %>% 
    best_elev_legend(hide_color_legend = hide_color_legend)
  if (add_elevation_labels) {
    base <- add_elevation_labels(
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
#' @seealso [plot_species_or_elevation()], [plot_elevation()],
#'   [plot_each_species()], [ggplot2::ggplot()].
#'
#' @family functions to create or modify plot layers
#' 
#' @export
#' @examples 
#' # Small dataset with a few species for quick examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
#' elevation <- fgeo.data::luquillo_elevation
#' 
#' # These functions look alike but internally they are not.
#' base_census <- plot_base_census(census)
#' base_census
#' base_elevation <- plot_base_elevation(elevation)
#' base_elevation
#' 
#' # Use plot_base_census() as a base for layers that need census data
#' add_species(base_census)
#' # This fails because the base plot has no information about elevation
#' \dontrun{
#'   add_elevation_contrours(base_census)
#' }
#' 
#' # Use plot_base_elevation() as a base for layers that need elevation data
#' add_elevation_contrours(base_elevation)
#' # This fails because the base plot has no information about species
#' \dontrun{
#'   add_species(base_elevation)
#' }
plot_base_elevation <- function(data) {
  data <- fgeo.tool::fgeo_elevation(data)
  ggplot(data, aes(gx, gy, z = elev))
}

#' @rdname plot_base_elevation
#' @export
plot_base_census <- function(data) {
  check_plot_base_census(data)
  
  ggplot(data, aes(gx, gy))
}

# Limits ------------------------------------------------------------------

#' Set the map limits.
#' 
#' @template p
#' @template xlim_ylim
#' 
#' @seealso [plot_species_or_elevation()], [plot_each_species()],
#'   [ggplot2::coord_fixed()].
#' 
#' @family functions to create or modify plot layers
#' 
#' @export
#' @examples 
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_stem5_random, sp %in% some_sp)
#' p <- plot_base_census(census)
#' p
#' 
#' axis_limits(p, xlim = c(0, 100), ylim = c(0, 400))
#' 
#' fgeo.data::luquillo_elevation %>% 
#'   plot_base_elevation() %>% 
#'   axis_limits(xlim = c(0, 100), ylim = c(0, 400))
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

# Layers ------------------------------------------------------------------

#' Represent species with points.
#' 
#' @template p
#' @template data_ggplot
#' @param fill Character; either a colour or "sp", which maps each species to a
#'   different color.
#' @template shape_point_size
#' 
#' @seealso [plot_species_or_elevation()], [plot_each_species()],
#'   [ggplot2::geom_point()].
#' 
#' @family functions to create or modify plot layers
#' 
#' @export
#' @examples 
#' # Small dataset with a few species for quick examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
#' p <- plot_base_census(census)
#' 
#' add_species(p)
#' 
#' add_species(p, fill = "white", shape = 22, point_size = 4)
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

#' Represent elevation with lines.
#' 
#' @template p
#' @param contour_size A number giving the size of the contour of elevation
#'   lines. Passed to `ggplot2::stat_contour()` (see [ggplot2::geom_contour()]).
#' @template low_high
#' @param bins A number giving the number of elevation lines to map.
#' 
#' @seealso [plot_each_species()], [plot_species_or_elevation()], [plot_elevation()], 
#'   [ggplot2::geom_contour()].
#' 
#' @family functions to create or modify plot layers
#' @export
#' @examples 
#' p <- plot_base_elevation(fgeo.data::luquillo_elevation)
#' add_elevation_contrours(p)
#' add_elevation_contrours(p, contour_size = 0.5, low = "grey", high = "black", bins = 4)
add_elevation_contrours <- function(p, 
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

#' Label elevation lines.
#' 
#' @template p
#' @template label_size_label_color_fontface
#' @param xyjust A number to adjust the position of the text labels of the 
#'   elevation lines.
#'
#' @seealso [plot_each_species()], [plot_species_or_elevation()],
#'   [plot_elevation()], [ggplot2::geom_text()].
#' 
#' @family functions to create or modify plot layers
#' 
#' @export
#' @examples 
#' elevation <- fgeo.data::luquillo_elevation
#' 
#' add_elevation_contrours(plot_base_elevation(elevation))
#' 
#' elevation %>%
#'   plot_base_elevation() %>% 
#'   add_elevation_contrours() %>%
#'   add_elevation_labels(label_size = 2, label_color = "black", xyjust = -0.25)
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
#' `hide_color_legend` hides the color legend, for example, of elevation lines.
#' 
#' @template p
#' 
#' @seealso [ggplot2::labs()],  [ggplot2::guides()].
#' 
#' @family functions to create or modify plot layers
#' 
#' @examples 
#' elevation <- fgeo.data::luquillo_elevation
#' p <- plot_base_elevation(elevation)
#' hide_axis_labels(p)
#' 
#' elevation %>% 
#'   plot_base_elevation() %>% 
#'   add_elevation_contrours() %>% 
#'   hide_color_legend()
#' @name hide 
NULL

#' @rdname hide
#' @export
hide_axis_labels <- function(p) {
  p + labs(x = NULL, y = NULL)
}

#' @rdname hide
#' @export
hide_color_legend <- function(p) {
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
#' 
#' @family functions to create or modify plot layers
#' 
#' @export
#' @examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_stem5_random, sp %in% some_sp)
#' p <- add_species(plot_base_census(census))
#' p
#' 
#' facet(p, "sp")
#' # Same
#' facet(p, ~sp)
#' 
#' facet(p, c("sp", "status"))
#' 
#' # Same, all in one step
#' census %>% 
#'   plot_base_census() %>% 
#'   add_species() %>% 
#'   facet(c("sp", "status"))
facet <- function(p, facets, ...) {
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

best_layout <- function(p, facet = FALSE) {
  if (!facet) {
    return(p)
  } else {
    facet(p, "sp")
  }
}

best_elev_legend <- function(p, hide_color_legend = FALSE) {
  if (!hide_color_legend) {
    return(p)
  } else {
    hide_color_legend(p)
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
  check_crucial_names(census, c("gx", "gy", "sp"))
  if (length(species) == 0) {
    rlang::abort("`sp` must be not empty.")
  }
  invisible(census)
}

check_add_species <- function(p, data) {
  p_data_laks_sp <- !any(grepl("sp", names(p[["data"]])))
  if (all(is.null(data), p_data_laks_sp)) {
    msg <- paste0(
      "The plot fed to `add_species()` lacks the variable `sp`\n",
      "* Did you forget to provide census data?"
    )
    abort(msg)
  }
  invisible(p)
}

check_add_elevation_labels <- function(p,
                                       label_color,
                                       label_size,
                                       xyjust,
                                       fontface) {
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
      "* Did you forget to call `add_elevation_contrours()` before `level_elev`?"
    )
    abort(msg)
  }
  invisible(p)
}

check_plot_base_census <- function(data) {
  if (rlang::has_name(data, "elev")) {
    msg <- paste0(
      "This function is designed not for elevation but for census data.\n",
      "* If you plan to map elevation lines, use instead `plot_base_elevation()`."
    )
    warn(msg)
  }
  invisible(data)
}
