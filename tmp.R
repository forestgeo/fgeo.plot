library(ggplot2)
library(fgeo.tool)
load_all()

census <- fgeo.tool::top(bciex::bci12s7mini, sp, 2)
cns1sp <- top(census, sp)
elevation <- dplyr::rename(bciex::bci_elevation, gx = x, gy = y)

# If elevation is null use what I already have in mapply_sp, else:
# wrap this in a function and pass it to map() or map2(). I may need
# to change the name of data in map_elev and data in add_sp().
map_elev_then_sp <- function(elevation, 
                             census,
                             contour_size = 0.5,
                             low = "blue",
                             high = "red",
                             label_size = 3,
                             label_color = "grey",
                             fontface = "italic",
                             xlim = NULL,
                             ylim = NULL,
                             custom_theme = NULL,
                             fill = "black",
                             shape = 21,
                             size = 3,
                             hide_elev_legend = FALSE
                             ) {
  elev <- map_elev(
    data = elevation,
    contour_size = contour_size,
    low = low,
    high = high,
    label_size = label_size,
    label_color = label_color,
    fontface = fontface,
    xlim = xlim,
    ylim = ylim,
    custom_theme = custom_theme
  )
  p <- facet_wrap_sp(
    add_sp(elev, data = census, fill = fill, shape = shape, size = size)
  )
  
  if (!hide_elev_legend) {
    return(p)
  } else {
    hide_legend_elev(p)
  } 
}

map_elev_then_sp(elevation, cns1sp, hide_elev_legend = TRUE)



# TODO: size in add_sp should be point_size for consistency.


# The stuff below is likely not up to date --------------------------------









 
  map_gx_gy_elev(elev) %>% 
    best_theme(custom_theme = custom_theme) %>% 
    contour_elev(size = contour_size, low = low, high = high)
    label_elev(size = label_size)


  map_elev(custom_theme = theme_gray())  

  # add_sp(cns1sp)


  
  
  

spp <- unique(cns$sp)
mapply_sp(cns, spp[[1]], fill = "black")[[1]]

map_elev(elev, contour_size = 0.7, label_size = 3, 
  custom_theme = NULL)


map_sp2(cns, xlim = c(0, 500), custom_theme = theme_gray(),
  wrap = FALSE
  )



elev %>% 
  map_gx_gy_elev() %>% 
  limit_gx_gy() %>% 
  contour_elev() %>% 
  label_elev(size = 3, color = "grey", fontface = "italic") %>% 
  theme_default()
  

cns %>% 
  map_gx_gy() %>% 
  add_sp(cns, size = 5) %>% 
  facet_v_sp() %>% 
  theme_default() %>% 
  limit_gx_gy() %>% 
  hide_axis_labels() 

cns %>% 
  map_gx_gy() %>% 
  add_sp(cns, size = 5, fill = "white") %>% 
  facet_v_sp()
