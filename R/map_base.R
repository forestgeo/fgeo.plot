map_base <- function(x,
                     xlim = NULL,
                     ylim = NULL,
                     theme = theme_map_sp()) {
  check_map_base(x = x, xlim = xlim, ylim = ylim, theme = theme)

  # If limits are not given by the user, set limits based on entire dataset
  if (is.null(xlim)) {xlim <- c(0, max(x$gx, na.rm = TRUE))}
  if (is.null(ylim)) {ylim <- c(0, max(x$gy, na.rm = TRUE))}

  ggplot(x, aes(gx, gy)) +
    coord_fixed(xlim = xlim, ylim = ylim) +
    scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2], 20)) +
    scale_y_continuous(minor_breaks = seq(ylim[1], ylim[2], 20)) +
    labs(x = NULL, y = NULL) +
    theme
}

check_map_base <- function(x, xlim, ylim, theme) {
  stopifnot(is.data.frame(x))
  fgeo.tool::check_crucial_names(x, c("gx", "gy"))
  if (!is.null(xlim)) {stopifnot(xlim >= 0)}
  if (!is.null(ylim)) {stopifnot(ylim >= 0)}
  theme_has_class_theme <- any(grepl("theme", class(theme)))
  stopifnot(theme_has_class_theme)
}


