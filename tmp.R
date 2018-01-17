library(tidyverse)
library(fgeo)

vft <- sinharaja::sinh_vft %>%
  as_tibble() %>%
  top(PlotID)

vft %>%
  top(QuadratID) %>%
  map_tag() %>%
  first()





sbst %>%
  fgeo.tool::add_status_tree(status_a = "alive", status_d = "dead") %>%
  select(-status) %>% 
  unique() %>% 
  fgeo.tool::collapse_censusid()
  
