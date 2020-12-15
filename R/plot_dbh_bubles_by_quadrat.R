#' List dbh bubble-plots by quadrat (good for .pdf output).
#'
#' @template vft
#' @template title_quad
#' @template header
#' @template theme
#' @param lim_min,lim_max Minimum and maximum limits of the plot area.
#' @param subquadrat_side Length in meters of the side of a subquadrat.
#' @template tag_size
#' @template move_edge
#' @param status_d A character string indicating the value of the variable
#'   status that corresponds to dead stems.
#'
#' @return A list which each element is a plot of class ggplot.
#'
#' @examples
#' assert_is_installed("fgeo.x")
#'
#' # Create a small VieFullTable
#' first_n <- function(x, n) x %in% sort(unique(x))[1:n]
#' small_vft <- fgeo.x::vft_4quad %>%
#'   dplyr::filter(first_n(CensusID, 1) & first_n(QuadratID, 2)) %>%
#'   dplyr::sample_n(50)
#'
#' plot_dbh_bubbles_by_quadrat(small_vft)
#'
#' # To print all plots into a .pdf file see `?pdf()`
#' plot_dbh_bubbles_by_quadrat(small_vft)
#'
#' # Be careful if subsetting by DBH: You may unintentionally remove dead trees.
#' # You should explicietly inlcude missing `DBH` values with `is.na(DBH)`
#' include_missing_dbh <- subset(small_vft, DBH > 20 | is.na(DBH))
#' plot_dbh_bubbles_by_quadrat(include_missing_dbh)
#'
#' # Customizing the maps ----------------------------------------------------
#' # A custom title and header
#' myheader <- paste(
#'   " ",
#'   "Head column 1                     Head column 2                          ",
#'   " ",
#'   " ........................................................................",
#'   " ........................................................................",
#'   sep = "\n"
#' )
#'
#' plot_dbh_bubbles_by_quadrat(
#'   small_vft,
#'   title_quad = "My Site, 2018. Quad:",
#'   header = myheader
#' )
#'
#' # Skip R CMD check for speed
#' \donttest{
#' # Tweak the theme with ggplot
#' library(ggplot2)
#'
#' plot_dbh_bubbles_by_quadrat(
#'   small_vft,
#'   title_quad = "My Site, 2018. Quad:",
#'   header = header_dbh_bubbles("spanish"),
#'   tag_size = 3,
#'   theme = theme_dbh_bubbles(
#'     axis.text = NULL, # NULL shows axis.text; element_blank() doesn't.
#'     plot.title = element_text(size = 15),
#'     plot.subtitle = element_text(size = 5),
#'     panel.background = element_rect(fill = "grey")
#'   )
#' )
#' }
#' @family plot functions
#' @family functions to list plots from ForestGEO ViewFullTable
#' @family functions to plot dbh bubbles
#' @export
plot_dbh_bubbles_by_quadrat <- function(vft,
                                        title_quad = "Site Name, YYYY, Quadrat:",
                                        header = header_dbh_bubbles(),
                                        theme = theme_dbh_bubbles(),
                                        lim_min = 0,
                                        lim_max = 20,
                                        subquadrat_side = 5,
                                        tag_size = 2,
                                        move_edge = 0,
                                        status_d = "dead") {
  .vft <- setNames(vft, tolower(names(vft)))
  core <- c(
    "plotid", "censusid", "tag", "dbh", "status", "quadratname",
    "qx", "qy"
  )
  crucial <- .vft[core]
  check_plot_dbh_bubbles_by_quadrat(
    vft = crucial,
    core = core,
    lim_min = lim_min,
    lim_max = lim_max,
    subquadrat_side = subquadrat_side,
    tag_size = tag_size,
    move_edge = move_edge,
    title_quad = title_quad,
    header = header,
    theme = theme
  )

  # Prepare
  message("* Appending tags of dead trees with the suffix '.d'")
  crucial$tagged_tag <- suffix_match(
    crucial$tag, crucial$status, status_d, ".d"
  )
  message("* Standarizing `dbh` by the count of `dbh` measurements")
  crucial$dbh_standarized <- as.numeric(crucial$dbh) / length(crucial$dbh)

  df_list <- split(crucial, crucial$quadratname)
  nms <- sort(unique(as.character(crucial$quadratname)))
  p <- lapply(
    df_list,
    map_quad_each,
    lim_min = lim_min,
    lim_max = lim_max,
    subquadrat_side = subquadrat_side,
    tag_size = tag_size,
    move_edge = move_edge,
    title_quad = title_quad,
    header = header,
    theme = theme
  )
  setNames(p, nms)
}

check_plot_dbh_bubbles_by_quadrat <- function(vft,
                                              core,
                                              lim_min,
                                              lim_max,
                                              subquadrat_side,
                                              tag_size,
                                              move_edge,
                                              title_quad,
                                              header,
                                              theme) {
  if (missing(vft)) stop("`vft` can't be missing")
  if (!is.data.frame(vft)) stop("`vft` should be a dataframe")
  stopifnot(
    is.numeric(lim_min), is.numeric(lim_max), is.numeric(subquadrat_side),
    is.numeric(tag_size), is.numeric(move_edge)
  )
  stopifnot(inherits(theme, "theme"))
  stopifnot(is.character(title_quad), is.character(header))
  check_crucial_names(vft, core)
  warn_multiple_plotid(vft)
  msg <- "censusid: Multiple values of treeid were detected."
  flag_if(vft, "censusid", is_multiple, warn, msg)

  invisible(vft)
}

map_quad_each <- function(.df,
                          lim_min,
                          lim_max,
                          subquadrat_side,
                          tag_size,
                          move_edge,
                          title_quad,
                          header,
                          theme) {
  # ggplots come with a default extention
  default_extention <- 1
  grid_adjust <- default_extention - move_edge
  quads <- stringr::str_pad(unique(.df$quadratname), width = 4, pad = 0)
  title_quad <- paste(title_quad, quads, sep = " ")
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
