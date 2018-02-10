# Few quadrats based on BCI ----------------------------------------------

library(bciex)
library(fgeo.tool)
library(fgeo.map)
library(dplyr)
filter <- dplyr::filter

all_quad <- bciex::bci12vft_mini %>%
  rename(QX = x, QY = y) %>%
  filter(PlotID == 1) %>%
  row_top(CensusID)

top4quad <- all_quad %>% row_top(QuadratID, 4)
use_data(top4quad, overwrite = TRUE)

top1quad <- all_quad %>% row_top(QuadratID, 1)
use_data(top1quad, overwrite = TRUE)
