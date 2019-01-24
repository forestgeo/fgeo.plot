# Internal implementation of functions to plot species and elevevation.

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
    X = cns_list, FUN = plot_sp_elev,
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

plot_sp_elev <- function(census,
                         elevation = NULL,
                         fill = "sp",
                         hide_fill_legend = FALSE,
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

  base <- base %>%
    add_species(census, fill = fill, shape = shape, point_size = point_size) %>%
    best_layout(facet = facet) %>%
    axis_limits(xlim = xlim, ylim = ylim) %>%
    best_theme(custom_theme = custom_theme)

  if (hide_fill_legend) base <- hide_fill_legend(base)

  base
}

plot_elev <- function(elevation,
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
    add_elevation_contours(
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

plot_base_elevation <- function(data) {
  data <- fgeo.tool::fgeo_elevation(data)
  ggplot(data, aes(gx, gy, z = elev))
}

plot_base_census <- function(data) {
  check_plot_base_census(data)

  ggplot(data, aes(gx, gy))
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
    lim <- c(0, maximum(coord))
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
      "* Did you forget to call `add_elevation_contours()` before `level_elev`?"
    )
    abort(msg)
  }
  invisible(p)
}

check_plot_base_census <- function(data) {
  if (rlang::has_name(data, "elev")) {
    msg <- paste0(
      "This function is designed not for elevation but for census data.\n",
      "* If you plan to plot elevation lines, use `plot_base_elevation()` instead."
    )
    warn(msg)
  }
  invisible(data)
}
