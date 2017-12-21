# map_sp ------------------------------------------------------------------

# For easier data manipulation
library(dplyr)
# Print only a few rows of tibbles (modern dataframes) to save space
options(dplyr.print_min = 6, dplyr.print_max = 6)

# Example data. Converting dataframe to tibble for better printing
census <- as_tibble(bciex::bci12t7mini)

map_sp(census, "hybapr")
x <- map_sp(census, "hybapr")
x



# map_quad() -------------------------------------------------------------

x <- mutate(four_quadrats, QuadratName = letters[QuadratName])
p <- map_quad(x, extend_grid = 0)
p

# map_tag() ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(map)
library(bciex)

# Subset of a public ViewFullTable from BCI (source:
# https://repository.si.edu/handle/10088/20925).

# Improve printing method
vft <- as_tibble(bci12vft_mini)
vft

# Filter the plot you want to map
vft1 <- dplyr::filter(vft, PlotID == 1)


# This data set has two wrong names that need to be fixed before using map_tag()
vft1_rnm <- dplyr::rename(vft1, qx = x, qy = y)
maps <- map_tag(vft1_rnm)

# Plotting only one map to screen
maps[1]



