# Data of a few quadrats of the census 7, in the ViewFullTable from Barro
# Colorado Island downloaded from: https://doi.org/10.5479/data.bci.20130603.
# This data is for examples.

library(tidyverse)

# After downloading the data from the link above, here I load all the data from
# a local repository


bci_vft <- tibble::as.tibble(
  readr::read_tsv("../bci/data-raw/ViewFullTable/ViewFullTable.txt")
)

# Filter only the last census to save space
bci12vft7 <- bci_vft %>%
  filter(bci_vft$PlotCensusNumber == 7)

quads <- unique(bci12vft7$QuadratName)[1:10]
bci12vft7mini <- bci12vft7 %>% filter(QuadratName %in% quads)

use_data(bci12vft7mini)

