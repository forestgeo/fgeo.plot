library(tidyverse)
library(fgeo)

vft <- sinharaja::sinh_vft %>%
  as_tibble() %>%
<<<<<<< HEAD
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
  
=======
  top(PlotID) %>%
  top(CensusID, -1) %>%
  select(Tag, Status, everything())

vft %>%
  # filter(Status == "alive") %>%
  # filter(Status == "dead") %>%
  top(QuadratID) %>%
  map_tag() %>%
  first()
>>>>>>> 25_single_status
