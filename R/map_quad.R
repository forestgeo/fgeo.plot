#' Map trees within a quadrat.
#'
#' @template vft
#' @template title_quad
#' @template header
#' @template theme
#' @param lim_min,lim_max Minimum and maximum limits of the plot area.
#' @param subquadrat_side Length in meters of the side of a subquadrat.
#' @template tag_size
#' @template extend_grid
#'
#' @return A list which each element is a plot of class ggplot.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(fgeo.utils)
#'
#' # Fixing wrong names
#' vft <- rename(bciex::bci12vft_mini, QX = x, QY = y)
#'
#' # Filter the data you want. For example:
#'
#' # Filtering trees of diameter greater than 10 cm from the last census of plot 1
#' # (see also ?fgeo.utils::rm_dead_twice)
#' want <- dplyr::filter(
#'   vft,
#'   DBH > 10,
#'   CensusID == max(CensusID, na.rm = TRUE),
#'   PlotID == 1
#' )
#'
#' # Filtering only two quadrats to save time
#' two_quadrats <- top(want, QuadratID, 2)
#' p <- map_quad(two_quadrats)
#'
#' # Visualizing only the first plot of `p`
#' first(p)
#'
#' # Printing all plots pf `p` to .pdf, with parameters optimized for size letter
#' tmp <- tempfile()  # Remplace this by somehtihing like "maps.pdf"
#'
#' pdf(tmp, paper = "letter", height = 10.5, width = 8)
#' p
#' dev.off()
#'
#' # Customizing the maps ----------------------------------------------------
#'
#' # Filtering only one quadrat to save time
#' one_quad <- top(want, QuadratID)
#'
#' # A custom title and header
#' myheader <- paste(
#'   " ",
#'   "Head column 1                     Head column 2                          ",
#'   " ",
#'   " ........................................................................",
#'   " ........................................................................",
#'   sep = "\n"
#' )
#' map_quad(one_quad, title_quad = "My Site, 2018. Quad:", header = myheader)
#'
#' # Many more tweaks are possible
#'
#' # Use functions from ggplot to tweak theme
#' library(ggplot2)
#'
#' map_quad(
#'   one_quad,
#'   title_quad = "My Site, 2018. Quad:",
#'   header = map_quad_header("spanish"),
#'   tag_size = 3,
#'   theme = theme_map_quad(
#'     axis.text = NULL,  # NULL shows axis.text; element_blank() doesn't.
#'     plot.title = element_text(size = 15),
#'     plot.subtitle = element_text(size = 5),
#'     panel.background = element_rect(fill = "grey")
#'   )
#' )
#'
map_quad <- function(vft,
                     title_quad = "Site Name, YYYY, Quadrat:",
                     header = map_quad_header(),
                     theme = theme_map_quad(),
                     lim_min = 0,
                     lim_max = 20,
                     subquadrat_side = 5,
                     tag_size = 2,
                     extend_grid = 0) {
  .vft <- setNames(vft, tolower(names(vft)))
  core <- c(
    "plotid", "censusid", "tag", "dbh", "status", "quadratname",
    "qx", "qy"
  )
  crucial <- .vft[core]
  check_map_quad(
    vft = crucial,
    core = core,
    lim_min = lim_min,
    lim_max = lim_max,
    subquadrat_side = subquadrat_side,
    tag_size = tag_size,
    extend_grid = extend_grid,
    title_quad = title_quad,
    header = header,
    theme = theme
  )

  # Prepare
  message("* Appending tags of dead trees with the suffix '.d'")
  crucial$tagged_tag <- tag_dead(crucial$tag, crucial$status)
  message("* Standarizing `dbh` by the count of `dbh` measurements")
  crucial$dbh_standarized <- crucial$dbh / length(crucial$dbh)

  df_list <- split(crucial, crucial$quadratname)
  p <- lapply(
    df_list,
    map_quad_each,
    lim_min = lim_min,
    lim_max = lim_max,
    subquadrat_side = subquadrat_side,
    tag_size = tag_size,
    extend_grid = extend_grid,
    title_quad = title_quad,
    header = header,
    theme = theme
  )
  nms <- sort(unique(as.character(crucial$quadratname)))
  setNames(p, sort(nms))
}

check_map_quad <- function(vft,
                           core,
                           lim_min,
                           lim_max,
                           subquadrat_side,
                           tag_size,
                           extend_grid,
                           title_quad,
                           header,
                           theme) {
  if (missing(vft)) stop("`vft` can't be missing")
  if (!is.data.frame(vft)) stop("`vft` should be a dataframe")
  stopifnot(
    is.numeric(lim_min), is.numeric(lim_max), is.numeric(subquadrat_side),
    is.numeric(tag_size), is.numeric(extend_grid)
  )
  arg_theme_has_class_theme <- any(grepl("theme", class(theme)))
  stopifnot(arg_theme_has_class_theme)
  stopifnot(is.character(title_quad), is.character(header))
  fgeo.utils::check_crucial_names(vft, core)
  check_unique_plotid(vft)
  check_unique_censusid(vft)
}

check_unique_plotid <- function(x) {
  msg <- "  * Filter your data to keep a single plot; then try again"
  fgeo.utils::check_unique(x, "plotid", "stop", msg)
  invisible(x)
}

check_unique_censusid <- function(x) {
  msg <- "  * Likely you should have filtered only the last `censusid`"
  fgeo.utils::check_unique(x, "censusid", "warning", msg)
  invisible(x)
}

map_quad_each <- function(.df,
                          lim_min,
                          lim_max,
                          subquadrat_side,
                          tag_size,
                          extend_grid,
                          title_quad,
                          header,
                          theme) {
  # ggplots come with a default extention
  default_extention <- 1
  grid_adjust <- default_extention - extend_grid

  title_quad <- paste(title_quad, unique(.df$quadratname), sep = " ")
  ggplot(.df, aes(x = qx, y = qy)) +
    geom_text_repel(aes(label = .df$tagged_tag), size = tag_size) +
    geom_point(aes(size = .df$dbh_standarized), shape = 1) +
    labs(title = title_quad, subtitle = header, x = NULL, y = NULL) +
    geom_vline(
      xintercept = seq(lim_min, lim_max, by = subquadrat_side),
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = seq(lim_min, lim_max, by = subquadrat_side),
      linetype = "dashed"
    ) +
    coord_fixed(
      xlim = c(lim_min + grid_adjust, lim_max - grid_adjust),
      ylim = c(lim_min + grid_adjust, lim_max - grid_adjust)
    ) +
    scale_x_continuous(breaks = lim_min:lim_max, sec.axis = dup_axis()) +
    scale_y_continuous(breaks = lim_min:lim_max, sec.axis = dup_axis()) +
    theme
}

#' Add the ending ".d" to the `Tag` of dead stems.
#'
#' @param x A character vector.
#' @param y A character vector giving the variable Status of a fgeo-table.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' tag_dead(
#'   x = c("tag1", "tag2"),
#'   y = c("dead", "whatever")
#' )
tag_dead <- function(x, y) {
  if (!all(is.character(x), is.character(y))) {
    stop("Both `x` and `y` must be character vectors", call. = FALSE)
  }
  if (!"dead" %in% y) {warning("No stem is dead. Is that what you expect?")}

  is_dead <- y == "dead"
  x[is_dead] <- paste0(x[is_dead], ".", sub("^(.).*$", "\\1", y[is_dead]))
  x
}
