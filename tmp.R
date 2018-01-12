library(tidyverse)
library(fgeo)

vft <- as_tibble(bciex::bci12vft_mini)
cns <- as_tibble(bciex::bci12s7mini) %>% add_lxly()

cns %>% select(matches("x")) %>% filter(lx >20)

x <- cns
x <- vft
