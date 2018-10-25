plot_base <- function(data, ...) {
  UseMethod("plot_base")
}

plot_base.default <- function(data, ...) {
  abort(paste0("Can't deal with data of class ", commas(class(data))))
}

plot_base.sp <- function(data, ...) {
  check_plot_base_census(data)
  ggplot(data, aes(gx, gy))
}

plot_base.elev <- function(data, ...) {
  data <- fgeo.tool::fgeo_elevation(data)
  ggplot(data, aes(gx, gy, z = elev))
}
