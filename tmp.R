library(tidyverse)
library(fgeo)

vft <- as_tibble(yosemite::ViewFullTable_yosemite)

x <- vft

suffix_edgy_dead(vft, "dead", ".d")
