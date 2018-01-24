library(ggplot2)
library(fgeo.tool)
load_all()

census <- fgeo.tool::top(bciex::bci12s7mini, sp, 2)
elevation <- bciex::bci_elevation


map_pure_elev(elevation, hide_legend_elev = TRUE)
map_elev(elevation, xlim = c(0, 500))

map_sp_elev(cns1sp, elevation, xlim = c(0, 500))


mapply_sp_elev(census, elevation, species = census$sp[[1]], xlim = c(0, 500))








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
  add_sp(cns, point_size = 5) %>% 
  facet_v_sp() %>% 
  theme_default() %>% 
  limit_gx_gy() %>% 
  hide_axis_labels() 

cns %>% 
  map_gx_gy() %>% 
  add_sp(cns, point_size = 5, fill = "white") %>% 
  facet_v_sp()
