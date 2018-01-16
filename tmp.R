library(tidyverse)
library(fgeo)

vft <- sinharaja::sinh_vft %>%
  as_tibble() %>%
  top(PlotID) %>%
  top(CensusID, -2)

vft %>%
  top(QuadratID) %>%
  map_tag() %>%
  first()


sbst %>%
  fgeo.tool::add_status_tree(status_a = "alive", status_d = "dead") %>% 
  select(-status) %>% 
  # top(censusid, -1) %>% 
  unique() %>%
  group_by(tag) %>% 
  mutate(n = length(tag)) %>% 
  filter(n > 1)
  
  
  
  check_unique_censusid() %>% 
  check_unique_tag()
