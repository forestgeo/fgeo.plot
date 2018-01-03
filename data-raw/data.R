# Few quadrats based on BCI ----------------------------------------------

library(bciex)
library(fgeo.utils)
library(map)
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



# Option dbh > 10

# Example data set
example_data <- bciex::bci12vft_mini %>%
  # Fix wrong names (https://goo.gl/Gvo1mn)
  rename(QX = x, QY = y)

# One plot
this_plotid <- 1
filtered_plotid <- filter(example_data, PlotID == this_plotid)

# Trees over 10 cm
limit <- 10
over_limit <- filter(filtered_plotid, DBH > limit)

# last cencus
last_census <- max(unique(over_limit$CensusID))
is_last_census <- over_limit$CensusID == last_census
over_limit <- over_limit[is_last_census, ]

# Some quadrats
set.seed(1)
four <- unique(over_limit$QuadratName)[1:4]
one <- unique(over_limit$QuadratName)[1]
four_quadrats <- filter(over_limit, QuadratName %in% four)
one_quadrat <- filter(over_limit, QuadratName  %in%  one)

# Saving data
use_data(four_quadrats, overwrite = TRUE)
use_data(one_quadrat, overwrite = TRUE)
