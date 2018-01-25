#' Map tree tags by status, showing four subquadrats per plot-page.
#'
#' This function maps tree tags by status and outputs a list of maps that can be
#' printed on a .pdf file. Each map shows four subquadrats within a quadrat. The
#' symbols on the map represent the status of each tree -- not the status of
#' each stem. Although you should likely provide data of only one or two
#' censuses, `maply_tag()` will summarize the data to reduce overplotting. The
#' data on the map summarizes the history of each stem accross all censuses
#' provided. Each tag will appear in the map only once or twice: * A tag will
#' appear once if it belongs to a tree which status was unique accross all
#' censuses provided -- either "alive" or "dead". * A tag will appear twice if
#' it belongs to a tree which status was "alive" in at least one census, and
#' also "dead" in at least one other census. This feature avoids unintentional
#' overplotting and makes interpreting the map easier.
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
#' @param bl,br,tr,tl Label each of the four maps of a quadrat. See [paginate()].
#' @template title_quad
#' @param show_page Logical; `FALSE` removes the page label from the map title.
#' @param show_subquad Logical; `FALSE` removes subquadrat labels on each map.
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
#' @family `maply_*` functions.
#' @seealso [graphics::points()], [ggplot2::geom_point()], [ggplot2::theme()]
#'   [map_tag_header()], [theme_map_tag()], [fgeo.tool::add_subquad()],
#'   [paginate()], [ggrepel::geom_text_repel].
#'
#' @section Acknowledgements:
#' Useful ideas and guidance came from Suzanne Lao, Stuart Davis, Shameema
#' Jafferjee Esufa, David Kenfack and Anudeep Singh. Andudeep Sinh also wrote
#' the algorithm to calculate subquadrats.
#'
#' @return A list of ggplots, where each element of the list is a map of tree
#'   tags by status, showing four subquadrats.
#' @export
#'
#' @examples
#' \dontrun{
#' library(fgeo.map)
#' library(fgeo.tool)
#' library(dplyr)
#'
#' # Filtering the data to map -----------------------------------------------
#'
#' # Filter the data you want. For example:
#' # (see ?top1quad)
#' want <- filter(
#'   top1quad,
#'   CensusID == 6,
#'   PlotID == 1
#' )
#' p <- map_tag(want)
#' # Visualizing only the first plot of `p`
#' first(p)
#' # Printing all plots of `p` to .pdf, with parameters optimized for size letter
#' pdf("map.pdf", paper = "letter", height = 10.5, width = 8)
#' p
#' dev.off()
#'
#' # Be careful if filtering by DBH: You may unintentionally remove dead trees.
#' # * Confirm this dataset has dead trees:
#' # (see `?top4quad`)
#' dead <- top4quad %>%
#'   add_status_tree(status_a = "alive", status_d = "dead") %>%
#'   top(QuadratID) %>%
#'   filter(status_tree == "dead")
#' select(dead, Tag, Status, status_tree, DBH)
#' first(map_tag(dead))
#' # * If you filter by `DBH`, you loose the dead trees becaue their `DBH = NA`
#' wrong <- filter(dead, DBH > 10)
#' wrong
#' # * The right way to do it is to explicietly inlcude rows where DBH = NA
#' right <- filter(dead, DBH > 10 | is.na(DBH))
#' right
#' first(map_quad(right))
#'
#' # Keeping dead trees with `is.na(DBH)` (e.g. tag 127885.d on the bottom right)
#' p <- filter(top4quad, DBH > 20 | is.na(DBH))
#' first(map_tag(p))
#'
#' # For more complex filtering, see also ?fgeo.tool::discard_dead_twice
#'
#' # Customizing the maps ----------------------------------------------------
#'
#' # Common tweaks
#'
#' p <- map_tag(top1quad, show_page = FALSE, show_subquad = FALSE)
#' first(p)
#'
#' p <- map_tag(
#'   top1quad,
#'   title_quad = "BCI 2012. Quadrat: ",
#'   bl = "bottom-left", br = "bottom-right", tr = "top-right", tl = "top-left",
#'   header = "Line 1: _________\nLine 2:\nLine 3:.....................",
#'   subquad_offset = -1,
#'   point_size = 3, point_shape = c(17, 6),
#'   tag_size = 2,
#'   move_edge = 0.5
#' )
#' first(p)
#'
#'
#'
#' # Themes
#'
#' library(ggplot2)
#'
#' p <- map_tag(top1quad, theme = theme_gray())
#' first(p)
#'
#' # Tweaking the default theme of map_tag()
#'
#' small_tweak <- theme_map_tag(legend.position = "bottom")
#' p <- map_tag(top1quad, theme = small_tweak)
#' first(p)
#'
#' large_tweak <- theme(
#'   legend.position = "bottom",
#'   legend.title = element_blank(),
#'   legend.text = element_text(size = 8, colour = "red"),
#'   text = element_text(size = 11, face = "bold.italic", colour = "white"),
#'   plot.background = element_rect(fill = "black"),
#'   plot.margin = margin(2, 2, 2, 2, "cm"),
#'   strip.background = element_rect(fill = "darkgreen"),
#'   strip.text = element_text(colour = "white"),
#'   # make grid to dissapear by matching background colour
#'   panel.background = element_rect(fill = "lightgreen"),
#'   panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
#'   panel.grid.major = element_line(colour = "black")
#' )
#' p <- map_tag(top1quad, theme = large_tweak)
#' first(p)
#'
#'
#'
#' # Quadrat and subquadrat dimensitons
#'
#' # Adapting the dimensions of quadrat and subquadrat to the range of qx and qy
#' # Creating new data set with QX and QY ranging 0-100
#' smaller <- top1quad
#' n <- nrow(smaller)
#' smaller$QX <- sample(0:10, n, replace = TRUE)
#' smaller$QY <- sample(0:10, n, replace = TRUE)
#'
#' p <- map_tag(smaller, x_q = 10, x_sq = 2.5)
#' first(p)
#'
#' # If limit-lines aren't visible, try extending the grid a little
#' p <- map_tag(smaller, x_q = 10, x_sq = 2.5, move_edge = 0.25)[1]
#' first(p)
#' }
maply_tag <- function(vft,
                    x_q = 20,
                    x_sq = 5,
                    y_q = x_q,
                    y_sq = x_sq,
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
                    header = map_tag_header(),
                    theme = theme_map_tag(),
                    move_edge = 0) {
  # Check
  .vft <- setNames(vft, tolower(names(vft)))
  crucial <- c("tag", "qx", "qy", "status", "quadratname", "censusid", "plotid")
  check_map_tag(
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
  sbst <- .vft[ , crucial]
  prepared <- prep_map_tag(
    sbst, x_q = x_q, x_sq = x_sq, y_q = y_q, y_sq = y_sq, subquad_offset =
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

check_map_tag <- function(.vft,
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
  if (dim(.vft)[1] == 0) {stop("Data can't have cero rows")}
  fgeo.tool::check_crucial_names(.vft, crucial)
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
  check_unique_plotid(.vft)
  
  msg_cnsid <- paste0(
    "* Likely you want only the last 2 censuses\n",
    "* Detected censuses: ", collapse(unique(.vft$censusid)),
    collapse = ""
  )
  fgeo.tool::check_unique(.vft, "censusid", "warning", msg_cnsid)
  
  check_max_print(.vft, "quadratname", times = 4)

  invisible(.vft)
}

prep_map_tag <- function(sbst,
                         x_q,
                         x_sq,
                         y_q,
                         y_sq,
                         subquad_offset,
                         bl,
                         br,
                         tr,
                         tl,
                         move_edge
                         ) {
  if (!is.character(sbst$quadratname)) {
    sbst$quadratname <- stringr::str_pad(sbst$quadratname, width = 4, pad = 0)
    rlang::warn("`quadratname` is not of class character. Pading with '0'.")
  }
  # Using the pipe (%>%) to avoid meaningless temporary-variables
  sbst %>%
    fgeo.tool::add_status_tree(status_a = "alive", status_d = "dead") %>%
    select(-.data$status) %>% 
    fgeo.tool::collapse_censusid() %>% 
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

#' Label the four divisions of a quadrat -- each with four subquadrats.
#'
#' This function makes it possible for [map_tag()] to plot each individual map.
#' Each map corresponds to one page and includes four subquadrats. There are a
#' total of four maps per quadrat (i.e. a total of 16 subquadrats per quadrat).
#'
#' @param x A dataframe with the variable `subquadrat`.
#' @param bl,br,tr,tl Number or character giving the label of the four
#'   subquadrats on each or the four divisions of a quadrat: bottom left (bl),
#'   bottom right (br), top right (tr), and top left (tl).
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
#' @keywords internal
#' @examples
#' \dontrun{
#' library(dplyr)
#' viewfulltable <- tribble(
#'    ~QX,  ~QY,
#'   17.9,    0,
#'    4.1,   15,
#'    6.1, 17.3
#' )
#' with_subquad <- fgeo.tool::add_subquad(viewfulltable, 20, 20, 5, 5)
#'
#' # Warning: Internal function
#' fgeo.map:::paginate(with_subquad)
#' fgeo.map:::paginate(with_subquad, "a", "b", "c", "d")
#' }
paginate <- function(x, bl = 1, br = 2, tr = 3, tl = 4, subquad_offset = NULL) {
  stopifnot(is.data.frame(x))
  fgeo.tool::check_crucial_names(x, "subquadrat")

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
#' @noRd
#' @param paged A dataframe with the variable `page`.
#' @param x_q Integer; the size in meters of a (squared) quadrat.
#' @param bl,br,tr,tl See `paginate()`.
#' @param move_edge A positive or negative number to move the edge lines of each map.
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
      page == tr ~ x_q  - mv_edge,
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
  if (show_page) {paste0(chr_quad, page)} else {chr_quad}
}

#' Create data to plot labels in each subquadrat.
#' @noRd
#' @examples
#' library(ggplot2)
#' library(tibble)
#'
#' tags <- tibble(
#'   tag = sample(1:10000, 100),
#'   qx = sample(1:20, 100, replace = TRUE),
#'   qy = sample(1:20, 100, replace = TRUE)
#' )
#'
#' df_labs <- df_subquad_labs(x_q = 20, y_q = 20, x_sq = 5, y_sq = 5)
#'
#' ggplot(data = tags, aes(qx, qy)) +
#'   geom_label(data = df_labs, aes(qx, qy, label = subquadrat),
#'     colour = "white", fill = "grey", fontface = "bold") +
#'   ggrepel::geom_text_repel(aes(label = tag))
df_subquad_labs <- function(x_q, x_sq, y_q, y_sq, subquad_offset, ...) {
  # Center labels in each subquadrat
  # x
  xoffset <- x_sq / 2
  xcentered <- seq(0, x_q, x_sq) + xoffset
  xtrimed <- xcentered[xcentered < x_q]  # remove tags beyond the range
  # y
  yoffset <- y_sq / 2
  ycentered <- seq(0, y_q, y_sq) + yoffset
  ytrimed <- ycentered[ycentered < y_q]  # remove tags beyond the range

  pos <- expand.grid(qx = xtrimed, qy = ytrimed, stringsAsFactors = FALSE)

  fgeo.tool::add_subquad(
    pos,
    x_q = x_q, x_sq = x_sq, y_q = y_q, y_sq = y_sq,
    subquad_offset = subquad_offset
  )
}

