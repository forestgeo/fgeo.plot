#' List plots of tree-tag status by subquadrat (good for .pdf output).
#'
#' This function plots tree tags by status and outputs a list of plots that can
#' be printed on a .pdf file. Each plot shows four subquadrats within a quadrat.
#' The symbols on the plot represent the status of each tree -- not the status of
#' each stem. Although you should likely provide data of only one or two
#' censuses, `plot_tag_status_by_subquadrat()` will summarize the data to reduce
#' overplotting. The data on the plot summarizes the history of each stem across
#' all censuses provided. Each tag will appear in the plot only once or twice:
#' * A tag will appear once if it belongs to a tree which status was unique
#' across all censuses provided -- either "alive" or "dead".
#' * A tag will appear twice if it belongs to a tree which status was "alive" in
#' at least one census, and also "dead" in at least one other census. This
#' feature avoids unintentional overplotting and makes interpreting the plot
#' easier.
#'
#' @template vft
#' @template x_q_y_q
#' @template x_sq_y_sq
#' @param subquad_offset `NULL` or `-1`. `NULL` defines the first column of
#'   subquadrats as 1.  `-1` defines the first column of subquadrats as 0.
#'   ```R
#'  subquad_offset = NULL    subquad_offset = -1
#'  ---------------------    -------------------
#'        14 24 34 44             04 14 24 34
#'        13 23 33 43             03 13 23 33
#'        12 22 32 42             02 12 22 32
#'        11 21 31 41             01 11 21 31
#'   ```
#' @param bl,br,tr,tl Number or character giving the label of the four
#'   subquadrats on each or the four divisions of a quadrat: bottom left (bl),
#'   bottom right (br), top right (tr), and top left (tl).
#' @template title_quad
#' @param show_page Logical; `FALSE` removes the page label from the plot title.
#' @param show_subquad Logical; `FALSE` removes subquadrat labels on each plot.
#' @param point_shape A vector of two numbers giving the shape of the points to
#'   plot (see possible shapes in the documentation of ?[graphics::points()],
#'   under the section entitled _'pch' values_).
#' @param point_size A number giving points size. Passed to
#'   [ggplot2::geom_point()].
#' @template tag_size
#' @template header
#' @template theme
#' @template move_edge
#'
#' @seealso [graphics::points()], [ggplot2::geom_point()], [ggplot2::theme()]
#'   [header_tag_status()], [theme_tag_status()], [fgeo.tool::add_subquad()],
#'   [ggrepel::geom_text_repel].
#'
#' @section Acknowledgment:
#' Useful ideas and guidance came from Suzanne Lao, Stuart Davis, Shameema
#' Jafferjee Esufali, David Kenfack and Anudeep Singh. Anudeep Sinh also wrote
#' the algorithm to calculate subquadrats.
#'
#' @template return_a_list_of_ggplots
#'
#' @examples
#' assert_is_installed("fgeo.x")
#'
#' # Create a small VieFullTable
#' first <- function(x) x %in% sort(unique(x))[1]
#' small_vft <- subset(fgeo.x::vft_4quad, first(CensusID) & first(QuadratID))
#'
#' p <- plot_tag_status_by_subquadrat(small_vft)
#' # Showing only two sub-quadtrats
#' p[1:2]
#'
#' # To print all plots into a .pdf file see `?pdf()`
#' plot_tag_status_by_subquadrat(small_vft)
#'
#' # Be careful if filtering by DBH: You may unintentionally remove dead trees.
#' # * If you filter by `DBH`, you loose the dead trees becaue their `DBH = NA`
#' # * You should explicietly inlcude missing DBH values with `is.na(DBH)`
#' include_missing_dbh <- subset(small_vft, DBH > 20 | is.na(DBH))
#' p <- plot_tag_status_by_subquadrat(include_missing_dbh)
#' # Showing only the first plot to keep the output short
#' p[[1]]
#'
#' # Customizing the maps ----------------------------------------------------
#' # Common tweaks
#' p <- plot_tag_status_by_subquadrat(
#'   small_vft,
#'   title_quad = "BCI 2012. Quadrat: ",
#'   bl = "bottom-left", br = "bottom-right", tr = "top-right", tl = "top-left",
#'   header = "Line 1: _________\nLine 2:\nLine 3:.....................",
#'   subquad_offset = -1,
#'   point_size = 3, point_shape = c(17, 6),
#'   tag_size = 2,
#'   move_edge = 0.5
#' )
#' p[[1]]
#'
#' # Skip R CMD check for speed
#' \donttest{
#' p <- plot_tag_status_by_subquadrat(
#'   small_vft,
#'   show_page = FALSE,
#'   show_subquad = FALSE
#' )
#' p[[1]]
#'
#' # Themes
#' library(ggplot2)
#'
#' p <- plot_tag_status_by_subquadrat(small_vft, theme = theme_gray())
#' p[[1]]
#'
#' # Tweaking the default theme of plot_tag_status_by_subquadrat()
#' # For many more options see ?ggplot2::theme
#' small_tweak <- theme_tag_status(legend.position = "bottom")
#' p <- plot_tag_status_by_subquadrat(small_vft, theme = small_tweak)
#' p[[1]]
#' }
#' @family plot functions
#' @family functions to list plots from ForestGEO ViewFullTable
#' @family functions to plot tag status
#' @export
plot_tag_status_by_subquadrat <- function(vft,
                                          x_q = 20,
                                          x_sq = 5,
                                          y_q = 20,
                                          y_sq = 5,
                                          subquad_offset = NULL,
                                          bl = 1,
                                          br = 2,
                                          tr = 3,
                                          tl = 4,
                                          title_quad = "Site Name, YYYY. Quadrat:",
                                          show_page = TRUE,
                                          show_subquad = TRUE,
                                          point_shape = c(19, 4),
                                          point_size = 1.5,
                                          tag_size = 3,
                                          header = header_tag_status(),
                                          theme = theme_tag_status(),
                                          move_edge = 0) {
  .vft <- setNames(vft, tolower(names(vft)))

  crucial <- c(
    "tag", "treeid", "status", "quadratname", "qx", "qy", "censusid", "plotid"
  )
  check_plot_tag_status_by_subquadrat(
    .vft = .vft,
    crucial = crucial,
    x_q = x_q,
    x_sq = x_sq,
    y_q = y_q,
    y_sq = y_sq,
    subquad_offset = subquad_offset,
    bl = bl,
    br = br,
    tr = tr,
    tl = tl,
    title_quad = title_quad,
    show_page = show_page,
    show_subquad = show_subquad,
    point_shape = point_shape,
    point_size = point_size,
    tag_size = tag_size,
    header = header,
    theme = theme,
    move_edge = move_edge
  )

  # Prepare
  sbst <- .vft[, crucial]
  prepared <- prep_plot_tag_status_by_subquadrat(
    sbst,
    x_q = x_q, x_sq = x_sq, y_q = y_q, y_sq = y_sq, subquad_offset =
      subquad_offset, bl = bl, br = br, tr = tr, tl = tl, move_edge = move_edge
  )

  # Plot
  df_list <- split(prepared, prepared$split)
  nms <- sort(unique(as.character(prepared$split)))
  p <- lapply(
    X = df_list,
    FUN = map_tag_each,
    x_q = x_q, x_sq = x_sq, y_q = y_q, y_sq = y_sq, subquad_offset =
      subquad_offset, title_quad = title_quad, show_page = show_page,
    show_subquad = show_subquad, point_shape = point_shape, point_size =
      point_size, tag_size = tag_size, header = header, theme = theme
  )
  setNames(p, nms)
}

check_plot_tag_status_by_subquadrat <- function(.vft,
                                                crucial,
                                                x_q,
                                                x_sq,
                                                y_q,
                                                y_sq,
                                                subquad_offset,
                                                bl,
                                                br,
                                                tr,
                                                tl,
                                                title_quad,
                                                show_page,
                                                show_subquad,
                                                point_shape,
                                                point_size,
                                                tag_size,
                                                header,
                                                theme,
                                                move_edge) {
  stopifnot(is.data.frame(.vft))
  if (dim(.vft)[1] == 0) {
    stop("Data can't have cero rows")
  }
  check_crucial_names(.vft, crucial)
  stopifnot(is.numeric(x_q))
  stopifnot(is.numeric(x_sq))
  stopifnot(is.numeric(y_q))
  stopifnot(is.numeric(y_sq))
  if (!is.null(subquad_offset)) stopifnot(subquad_offset == -1)
  stopifnot(length(bl) == 1)
  stopifnot(length(br) == 1)
  stopifnot(length(tr) == 1)
  stopifnot(length(tl) == 1)
  stopifnot(is.character(title_quad))
  stopifnot(is.logical(show_page))
  stopifnot(is.logical(show_subquad))
  stopifnot(is.numeric(point_shape))
  stopifnot(length(point_shape) == 2)
  stopifnot(is.numeric(point_size))
  stopifnot(length(point_size) == 1)
  stopifnot(is.numeric(tag_size))
  stopifnot(length(tag_size) == 1)
  stopifnot(is.character(header))
  arg_theme_is_of_class_theme <- any(grepl("theme", class(theme)))
  stopifnot(arg_theme_is_of_class_theme)
  stopifnot(is.numeric(move_edge))
  warn_multiple_plotid(.vft)

  msg_cnsid <- paste0(
    "* Likely you want only the last 2 censuses\n",
    "* Detected censuses: ", commas(unique(.vft$censusid)),
    collapse = ""
  )
  flag_if(.vft, "censusid", is_multiple, warn, msg_cnsid)

  check_max_print(.vft, "quadratname", times = 4)

  invisible(.vft)
}

prep_plot_tag_status_by_subquadrat <- function(sbst,
                                               x_q,
                                               x_sq,
                                               y_q,
                                               y_sq,
                                               subquad_offset,
                                               bl,
                                               br,
                                               tr,
                                               tl,
                                               move_edge) {
  # `type_ensure()` fails to pad with 0. This is a special kind of coersion
  if (!is.character(sbst$quadratname)) {
    sbst$quadratname <- stringr::str_pad(sbst$quadratname, width = 4, pad = 0)
    rlang::warn("`quadratname` is not of class character. Pading with '0'.")
  }

  # Ensure type
  chr_var <- c("tag", "status", "quadratname")
  sbst <- fgeo.tool::type_ensure(sbst, chr_var, "character")
  dbl_var <- c("qx", "qy")
  sbst <- fgeo.tool::type_ensure(sbst, dbl_var, "double")
  int_var <- c("censusid", "plotid")
  sbst <- fgeo.tool::type_ensure(sbst, int_var, "integer")

  # Using the pipe (%>%) to avoid meaningless temporary-variables
  sbst %>%
    fgeo.tool::add_status_tree(status_a = "alive", status_d = "dead") %>%
    select(-.data$status) %>%
    collapse_censusid() %>%
    fgeo.tool::add_subquad(
      x_q = x_q, x_sq = x_sq, y_q = y_q, y_sq = y_sq,
      subquad_offset = subquad_offset
    ) %>%
    group_by(.data$quadratname) %>%
    paginate(
      bl = bl, br = br, tr = tr, tl = tl,
      subquad_offset = subquad_offset
    ) %>%
    add_subquad_lims(
      x_q = x_q,
      bl = bl, br = br, tr = tr, tl = tl,
      move_edge = move_edge
    ) %>%
    mutate(split = paste(.data$quadratname, .data$page, sep = "_")) %>%
    ungroup() %>%
    unique()
}

#' Label the four divisions of a quadrat, each with four subquadrats.
#'
#' This function makes it possible for [plot_tag_status_by_subquadrat()] to
#' create each individual plot. Each plot corresponds to one page and includes
#' four subquadrats. There are a total of four plots per quadrat (i.e. a total
#' of 16 subquadrats per quadrat).
#'
#' @param x A dataframe with the variable `subquadrat`.
#' @param bl,br,tr,tl See [plot_tag_status_by_subquadrat()].
#' @param subquad_offset `NULL` or `-1`. This argument is for internal use. It
#'   helps paginate to use the same code for data with two different types of
#'   labels for the subquadrat variable: (1) that with subquadrat-columns
#'   starting at 1; and (2) that with subquadrat-columns starting at 0.
#'
#' ```R
#' The four divisions of a quadrat (bl, br, tr, tl), each with four subquadrats:
#'
#' tl       tr
#' ----     -----
#' 14 24    34 44
#' 13 23    33 43
#'
#' bl       br
#' ----     -----
#' 12 22    32 42
#' 11 21    31 41
#' ```
#' @return A modified version of the input with the additional variable `page`.
#' @seealso [fgeo.tool::add_subquad()].
#' @noRd
paginate <- function(x, bl = 1, br = 2, tr = 3, tl = 4, subquad_offset = NULL) {
  stopifnot(is.data.frame(x))
  check_crucial_names(x, "subquadrat")

  if (!is.null(subquad_offset)) {
    stopifnot(subquad_offset == -1)
    # If first column of subquadrats is not 1 (but 0): recode; run; recode back
    x <- fgeo.tool::recode_subquad(x, offset = 1)
  }
  w_page <- mutate(
    x,
    page = dplyr::case_when(
      subquadrat == 11 ~ bl,
      subquadrat == 12 ~ bl,
      subquadrat == 21 ~ bl,
      subquadrat == 22 ~ bl,

      subquadrat == 31 ~ br,
      subquadrat == 32 ~ br,
      subquadrat == 41 ~ br,
      subquadrat == 42 ~ br,

      subquadrat == 34 ~ tr,
      subquadrat == 33 ~ tr,
      subquadrat == 44 ~ tr,
      subquadrat == 43 ~ tr,

      subquadrat == 14 ~ tl,
      subquadrat == 13 ~ tl,
      subquadrat == 24 ~ tl,
      subquadrat == 23 ~ tl,
    )
  )

  if (!is.null(subquad_offset)) {
    w_page <- fgeo.tool::recode_subquad(w_page, offset = -1)
  }
  w_page
}

#' Add plot limits to a dataframe with the variable `subquadrat`.
#'
#' @param paged A dataframe with the variable `page`.
#' @param x_q Integer; the size in meters of a (squared) quadrat.
#' @param bl,br,tr,tl #' @param bl,br,tr,tl See
#'   [plot_tag_status_by_subquadrat()].
#' @param move_edge A positive or negative number to move the edge lines of each map.
#'
#' @noRd
add_subquad_lims <- function(paged, x_q = 20, bl, br, tr, tl, move_edge = 0) {
  # ggplots come with a default extention
  default_extention <- 0.45
  mv_edge <- default_extention - move_edge

  mutate(paged,
    x1 = dplyr::case_when(
      page == bl ~ 0 + mv_edge,
      page == br ~ (x_q / 2) + mv_edge,
      page == tr ~ (x_q / 2) + mv_edge,
      page == tl ~ 0 + mv_edge
    ),
    x2 = dplyr::case_when(
      page == bl ~ (x_q / 2) - mv_edge,
      page == br ~ x_q - mv_edge,
      page == tr ~ x_q - mv_edge,
      page == tl ~ (x_q / 2) - mv_edge
    ),
    y1 = dplyr::case_when(
      page == bl ~ 0 + mv_edge,
      page == br ~ 0 + mv_edge,
      page == tr ~ (x_q / 2) + mv_edge,
      page == tl ~ (x_q / 2) + mv_edge
    ),
    y2 = dplyr::case_when(
      page == bl ~ (x_q / 2) - mv_edge,
      page == br ~ (x_q / 2) - mv_edge,
      page == tr ~ x_q - mv_edge,
      page == tl ~ x_q - mv_edge
    )
  )
}

map_tag_each <- function(prep_df,
                         subquad_offset,
                         title_quad,
                         show_page,
                         show_subquad,
                         point_shape,
                         point_size,
                         tag_size,
                         header,
                         theme,
                         x_q,
                         y_q,
                         x_sq,
                         y_sq) {
  # Data to plot labels on map
  lab_df <- df_subquad_labs(
    x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq,
    subquad_offset = subquad_offset
  )
  # Allow plotting labels with `shape` mapping to `status_tree`
  lab_df$status_tree <- NA

  # >>>
  # If in this chunk I refer to `var` as `df$var` I get this error:
  #   Error: Aesthetics must be either length 1 or the same as the data (16):
  #   x, y, label, shape
  base <- ggplot(data = prep_df, aes(x = qx, y = qy, shape = status_tree)) +
    scale_shape_manual(values = curate_point_shape(prep_df, point_shape))
  if (show_subquad) {
    base <- base +
      geom_label(
        data = lab_df,
        aes(qx, qy, label = subquadrat),
        colour = "white", fill = "#f4f2f2", fontface = "bold", size = 12
      )
  }
  # <<<
  .caption <- caption_edge_tag(prep_df, x_q = x_q, y_q = y_q)
  if (is.null(.caption)) .caption <- "None."
  base +
    geom_point(size = point_size) +
    ggrepel::geom_text_repel(aes(label = prep_df$tag), size = tag_size) +
    scale_x_continuous(
      minor_breaks = seq(1, x_q, 1), breaks = seq(0, x_q, x_sq)
    ) +
    scale_y_continuous(
      minor_breaks = seq(1, y_q, 1), breaks = seq(0, y_q, y_sq)
    ) +
    coord_fixed(
      xlim = c(unique(prep_df$x1), unique(prep_df$x2)),
      ylim = c(unique(prep_df$y1), unique(prep_df$y2))
    ) +
    labs(
      title = entitle_map(prep_df, title_quad, show_page = show_page),
      subtitle = header,
      x = NULL, y = NULL,
      caption = paste0("\n\nSpillover: ", .caption)
    ) +
    theme
}

curate_point_shape <- function(x,
                               point_shape,
                               status_a = "alive",
                               status_d = "dead") {
  if (identical(sort(unique(x$status_tree)), sort(c(status_a, status_d)))) {
    return(point_shape)
  }
  if (unique(x$status_tree) == status_a) {
    return(point_shape[[1]])
  }
  if (unique(x$status_tree) == status_d) {
    return(point_shape[[2]])
  }
}

entitle_map <- function(x, chr, show_page = TRUE) {
  quad <- stringr::str_pad(unique(x$quadratname), width = 4, pad = 0)
  chr_quad <- paste0(chr, " ", quad)
  page <- paste0(" (", unique(x$page), ")")
  if (show_page) {
    paste0(chr_quad, page)
  } else {
    chr_quad
  }
}

#' Create data to plot labels in each subquadrat.
#'
#' @examples
#' library(ggplot2)
#'
#' tags <- dplyr::tibble(
#'   tag = sample(1:10000, 100),
#'   qx = sample(1:20, 100, replace = TRUE),
#'   qy = sample(1:20, 100, replace = TRUE)
#' )
#'
#' df_labs <- df_subquad_labs(x_q = 20, y_q = 20, x_sq = 5, y_sq = 5)
#'
#' ggplot(data = tags, aes(qx, qy)) +
#'   geom_label(
#'     data = df_labs, aes(qx, qy, label = subquadrat),
#'     colour = "white", fill = "grey", fontface = "bold"
#'   ) +
#'   ggrepel::geom_text_repel(aes(label = tag))
#' @noRd
df_subquad_labs <- function(x_q, x_sq, y_q, y_sq, subquad_offset, ...) {
  # Center labels in each subquadrat
  # x
  xoffset <- x_sq / 2
  xcentered <- seq(0, x_q, x_sq) + xoffset
  xtrimed <- xcentered[xcentered < x_q] # remove tags beyond the range
  # y
  yoffset <- y_sq / 2
  ycentered <- seq(0, y_q, y_sq) + yoffset
  ytrimed <- ycentered[ycentered < y_q] # remove tags beyond the range

  pos <- expand.grid(qx = xtrimed, qy = ytrimed, stringsAsFactors = FALSE)

  fgeo.tool::add_subquad(
    pos,
    x_q = x_q, x_sq = x_sq, y_q = y_q, y_sq = y_sq,
    subquad_offset = subquad_offset
  )
}
