library(tidyverse)
library(fgeo)

vft <- sinharaja::sinh_vft %>%
  as_tibble() %>%
  top(PlotID) %>%
  top(CensusID, -1) %>%
  select(Tag, Status, everything())

vft %>%
  # filter(Status == "alive") %>%
  # filter(Status == "dead") %>%
  top(QuadratID) %>%
  map_tag() %>%
  first()
