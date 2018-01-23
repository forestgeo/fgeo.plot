library(ggplot2)

cns <- fgeo.tool::top(bciex::bci12s7mini, sp, 2)
elev <- dplyr::rename(bciex::bci_elevation, gx = x, gy = y)



map_elev(elev, contour_size = 0.7, label_size = 3, 
  custom_theme = NULL)

map_sp2(cns, xlim = c(0, 500), custom_theme = theme_gray())

map_sp2(cns, xlim = c(0, 500), custom_theme = theme_gray(),
  wrap = FALSE
  )

spp <- unique(cns$sp)
mapply_sp(cns, spp, fill = "black")[[1]]


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
