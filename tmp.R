census <- fgeo.tool::top(bciex::bci12s7mini, sp, 2)

elevation <- bciex::bci_elevation
head(elevation)
elevation <- fgeo.tool::restructure_elev(bciex::bci_elevation)
head(elevation)

# Showing first plot only.
p <- mapply_sp_elev(census)[[1]]
p[[1]]


p <- mapply_sp_elev(
  census,
  elevation,
  species = spp,
  fill = "white",
  shape = 21,
  point_size = 5,
  contour_size = 1,
  low = "grey",
  high = "black",
  hide_legend_elev = TRUE,
  bins = 7,
  label_elev = FALSE
)
p[[1]]

# Same but outputs a plot, not a list of plots
map_sp_elev(census, elevation)

# Similar but maps elevation exclusively
map_elev(elevation)

# For maximum control, you can compose maps as you like
map_gx_gy_elev(elevation) %>%
  limit_gx_gy(xlim = c(0, 1200)) %>%
  contour_elev(contour_size = 0.5) %>%
  label_elev(label_color = "red") %>%
  hide_axis_labels() %>%
  hide_legend_elev() %>%
  add_sp(census, point_size = 5) %>%
  facet_h_sp() %>%
  theme_default(legend.position = "top")
