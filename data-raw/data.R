# Few quadrats based on BCI ----------------------------------------------

library(bciex)
library(fgeo.utils)
library(fgeo.map)
library(dplyr)
filter <- dplyr::filter

all_quad <- bciex::bci12vft_mini %>%
  rename(QX = x, QY = y) %>%
  filter(PlotID == 1) %>%
  top(CensusID)

top4quad <- all_quad %>% top(QuadratID, 4)
use_data(top4quad, overwrite = TRUE)

top1quad <- all_quad %>% top(QuadratID, 1)
use_data(top1quad, overwrite = TRUE)
