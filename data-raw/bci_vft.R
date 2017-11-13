# ViewFullTable with data of a few quadrats of the 50 hectare plot from Barro
# Colorado Island downloaded from: https://doi.org/10.5479/data.bci.20130603.
# Explanation by Suzanne Lao https://goo.gl/fg2nqh.

# 1. Download (from https://doi.org/10.5479/data.bci.20130603) and store locally

library(tidyverse)

# Load from local
bci_vft <- tibble::as.tibble(
  readr::read_tsv("../bci/data-raw/ViewFullTable/ViewFullTable.txt")
)

# Censuses 6 and 7 of the 50 hectare plot
bci12vft_mini <- bci_vft %>%
  filter(
    PlotID == 1,
    PlotCensusNumber %in% c(6, 7),
    !is.na(QuadratName),
  ) %>%
  filter(QuadratName %in% unique(.$QuadratName)[1:10])

use_data(bci12vft_mini)


