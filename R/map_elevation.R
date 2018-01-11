map_elevation <- function(elevation,
                          xlim = NULL,
                          ylim = NULL,
                          theme = theme_map_sp(),
                          line_size = 0.5,
                          low = "#132B43",
                          high = "#56B1F7",
                          bins = NULL) {
  check_map_base(x = elevation, xlim = xlim, ylim = ylim, theme = theme)
  
  p <- map_base(x = elevation, xlim = xlim, ylim = ylim, theme = theme)
  
  add_elev(
    p,
    elevation = elevation,
    line_size = line_size,
    bins = bins,
    high = high,
    low = low
  )
}

add_elev <- function(p,
                     elevation,
                     line_size = 0.5,
                     low = "#132B43",
                     high = "#56B1F7",
                     bins = NULL) {
  check_add_elev(
    p = p, 
    elevation = elevation,
    line_size = line_size,
    low = low,
    high = high,
    bins = bins
  )
  
  p_elev <- p +
    stat_contour(
      data = elevation,
      aes(x = gx, y = gy, z = elev, colour = ..level..),
      size = line_size,
      bins = bins
    ) +
    scale_colour_continuous(low = low, high = high)
  # p_elev references elevation on a legend to the right of the plot. Not nice.
  reference_elev_on_map(p_elev)
}

reference_elev_on_map <- function(p) {
  label_properties <-   list(
    "far.from.others.borders", "calc.boxes",
    "enlarge.box", box.color = NA, fill = "transparent", "draw.rects"
  )
  directlabels::direct.label(p, label_properties)
}



# Check -------------------------------------------------------------------

check_add_elev <- function(p, elevation, line_size, low, high, bins) {
  p_has_class_ggplot <- any(grepl("ggplot", class(p)))
  stopifnot(p_has_class_ggplot)
  elevation_is_dataframe <- any(grepl("data.frame", class(elevation)))
  stopifnot(elevation_is_dataframe)
  fgeo.tool::check_crucial_names(elevation, c("gx", "gy", "elev"))
  stopifnot(is.numeric(line_size))
  stopifnot(is.character(low))
  stopifnot(is.character(high))
  if (!is.null(bins)) {stopifnot(is.numeric(bins))}
}
