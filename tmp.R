
library(dplyr)
library(ggplot2)
library(try)

# Subset of a public ViewFullTable from BCI (source:
# https://repository.si.edu/handle/10088/20925).

# data("bci12vft_mini")

# Improve printing method
vft <- as_tibble(bci12vft_mini)
vft

# Filter the plot you want to map
vft1 <- dplyr::filter(vft, PlotID == 1)

# Another look
glimpse(vft1)

# Creating all maps but plotting to screen only one map
maps <- map_tag(vft1)
maps[1]

# Printing only maps 1-4 to a .pdf
pdf("default-map.pdf", paper = "a4")
maps[1:4]
dev.off()

# Common customization (printing only 1 map to screen)
map_tag(vft1_rnm,
        site_name = "BCI 2012", point_size = 3, point_shape = c(17, 6), tag_size = 5
)[1]

# Custom header
map_tag(vft1_rnm, site_name = "BCI 2012",
        header = "Line 1: _________\nLine 2:\nLine 3:....................."
)[1]

# Maybe easier
your_header <- get_header(
  line1 = "Your header-line 1: _____________________________",
  line2 = "Your header-line 3: _____________________________",
  line3 = "Your header-line 2: _____________________________"
)
map_tag(vft1_rnm, site_name = "BCI 2012", header = your_header)[1]

# Custom theme: using a pre-made theme
# Allow using pre-made themes (e.g. ggplot2::theme_bw()) and building custom
# themes (with ggplot::theme()).
library(ggplot2)

map_tag(vft1_rnm, site_name = "BCI 2012", theme = theme_gray())[1]

# Custom theme: using a pre-made theme
# An extreeme example to show that themes are extreemely flexible
your_theme <- ggplot2::theme(
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(size = 8, colour = "red"),
  text = element_text(size = 11, face = "bold.italic", colour = "white"),
  plot.background = element_rect(fill = "black"),
  plot.margin = margin(2, 2, 2, 2, "cm"),
  strip.background = element_rect(fill = "darkgreen"),
  strip.text = element_text(colour = "white"),
  # make grid to dissapear by matching background colour
  panel.background = element_rect(fill = "lightgreen"),
  panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
  panel.grid.major = element_line(colour = "black")
)
map_tag(vft1_rnm, site_name = "BCI 2012", theme = your_theme)[1]

# Adapting the dimensions of quadrat and subquadrat to the range of x and y
# Creating new data set with x and y ranging 0-100
smaller <- vft1_rnm
n <- nrow(smaller)
smaller$x <- sample(0:10, n, replace = TRUE)
smaller$y <- sample(0:10, n, replace = TRUE)

map_tag(smaller, x_q = 10, x_sq = 2.5)[1]
# If limit-lines aren't visible, try extending the grid a little
map_tag(smaller, x_q = 10, x_sq = 2.5, extend_grid = 0.25)[1]



# Version used to plot labels for David

# plot_repulsive_tags <- function(prep_df,
#                                 site_name,
#                                 point_shape,
#                                 point_size,
#                                 tag_size,
#                                 header,
#                                 theme,
#                                 dim_x = 20,
#                                 dim_y = 20,
#                                 div_x = 5,
#                                 div_y = 5) {
#   assertive::assert_is_data.frame(prep_df)
#   assertive::assert_is_character(site_name)
#
#   # Data to plot labels on map
#   lab_df <- df_labels(dim_x = dim_x, dim_y = dim_y, div_x = div_x, div_y= div_y)
#   lab_df <- dplyr::rename(lab_df, lx = qx, ly = qy)
#   # let lab_df be used on a ggplot mapping to shape = latest_tree_status
#   lab_df$latest_tree_status <- NA
#
#   id_quadrat_subquadrat <- unique(prep_df$id)
#   ggplot2::ggplot(
#     prep_df, ggplot2::aes(x = lx, y = ly, shape = latest_tree_status)
#   ) +
#     ggplot2::scale_shape_manual(values = point_shape) +
#     ggplot2::geom_label(data = lab_df, aes(lx, ly, label = subquadrat),
#       colour = "white", fill = "grey", fontface = "bold"
#     ) +
#     ggplot2::geom_point(size = point_size) +
#     ggrepel::geom_text_repel(ggplot2::aes(label = tag), size = tag_size) +
#     ggplot2::scale_x_continuous(
#       minor_breaks = seq(1, dim_x, 1), breaks = seq(0, dim_x, div_x)
#     ) +
#     ggplot2::scale_y_continuous(
#       minor_breaks = seq(1, dim_y, 1), breaks = seq(0, dim_y, div_y)
#     ) +
#     ggplot2::coord_fixed(
#       xlim = c(unique(prep_df$x1), unique(prep_df$x2)),
#       ylim = c(unique(prep_df$y1), unique(prep_df$y2))
#     ) +
#     # coord_fixed() +
#     ggplot2::labs(x = NULL, y = NULL) +
#     ggplot2::labs(
#       title = paste0(site_name, ". ", id_quadrat_subquadrat),
#       subtitle = header
#     ) +
#     theme
# }
