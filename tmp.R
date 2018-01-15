library(tidyverse)
fgeo <- tibble(
  tag = str_pad(as.character(1:10), 4, pad = "0"),
  qx = 1:10, 
  qy = 1:10
)
fgeo

x_coord <- fgeo$qx
y_coord <- fgeo$qy
x <- fgeo$tag
max_n = 5



caption_var_xy(x, x_coord, y_coord, 5)




ggplot(x, aes(qx, qy)) +
  geom_point() +
  labs(caption = "hi")
