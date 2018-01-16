x <- tibble::tribble(
  ~CensusID, ~Tag, ~Status,
          1,    1, "alive",
          1,    1,  "dead",
          1,    2,  "dead",
          1,    2,  "dead",
          2,    1, "alive",
          2,    1, "alive",
          2,    2, "alive",
          2,    2,  "dead"
)
add_status_tree(x, status_a = "alive", status_d ="dead") %>%
  mutate(CensusID = collapse(unique(CensusID))) %>% 
  unique()











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





