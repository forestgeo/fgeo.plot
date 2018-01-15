library(tidyverse)
library(fgeo)

vft <- sinharaja::sinh_vft %>% 
  as_tibble() %>% 
  top(PlotID) %>% 
  top(CensusID, -2) %>% 
  filter(Status == "dead") %>% 
  filter(QX > 20 | QY > 20) %>% 
  arrange(QuadratID, QX, QY) %>% 
  select(QuadratID, QX, QY, Tag, Status, everything())
vft

quad_spillover <- vft %>% 
  count(QuadratID) %>%
  arrange(desc(n)) %>% 
  .[1, ] %>% 
  pull(QuadratID)
  pull(QuadratID)[[1]]
  filter(QuadratID == 489) 

demo <- vft %>% dplyr::filter(QuadratID == quad_spillover)
p <- map_tag(demo)

pdf("map.pdf", paper = "letter", height = 10.5, width = 8)
p[2]
dev.off()
