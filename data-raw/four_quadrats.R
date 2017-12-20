library(dplyr)

# Example data set
example_data <- bciex::bci12vft_mini %>%
  # Fix wrong names (https://goo.gl/Gvo1mn)
  rename(QX = x, QY = y)

this_plotid <- 1
filtered_plotid <- filter(example_data, PlotID == this_plotid)

limit <- 10
over_limit <- filter(filtered_plotid, DBH > limit)
# Silent means that filtering was successful
check_dbh(over_limit$DBH, limit)

if (length(unique(over_limit$CensusID)) > 1) {
  warning(
    "Multiple censuses were detected\n",
    "  * Filtering only the greatest `CensusID`"
  )
  last_census <- max(unique(over_limit$CensusID))
  is_last_census <- over_limit$CensusID == last_census
  over_limit <- over_limit[is_last_census, ]
}

# Choosing one quadrat at random
this_quadrat <- "4919"
one_quadrat <- filter(over_limit, QuadratName == this_quadrat)

one_quadrat <- dplyr::mutate(
  one_quadrat,
  # Tag dead trees
  tagged_tag = tag_dead(Tag, Status),
  # Standarize the size of the points by the total number of trees
  dbh_standarized = DBH / length(DBH)
)

one_quadrat$QuadratName <- sample(
  1:4, length(one_quadrat$QuadratName), replace = TRUE
)

four_quadrats <- one_quadrat
use_data(four_quadrats)
