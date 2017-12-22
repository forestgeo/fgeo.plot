# Trees that were found dead for the first time in the last census --------

#' Exclude trees that were found dead in the last and previous last censuses.
#'
#' Excludes trees that were found dead in the last and previous last censuses.
#' Said another way, this function keeps trees that were found in one of the
#' last two censuses.
#'
#' @param x
#' @param cns
#'
#' @return
#' @export
#'
#' @examples
rm_dead_twice <- function(x, cns = "plotcensusnumber") {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(cns))
  check_crucial_names(x, c(cns, "tag", "status"))

  # xxx rethink about this because I'm unsure if the status of a tree here is
  # not considering that there are more then one census
  x <-  add_status_tree(x)
  x$cns <- x[[cns]]
  last <- max(x$cns, na.rm = TRUE)
  if (last <= 1) {
    warning("`x` has less than two censuses. Returning `x` as is")
  }
  last2 <- x[x$cns %in% c(last, last - 1), ]
  by_tag <- dplyr::group_by(last2, tag)
  smry <- dplyr::summarize(by_tag, keep = !identical(status_tree, c("dead", "dead")))
  rm_dead_twice <- smry[smry$keep, ]$tag
  x$cns <- NULL
  x[x$tag %in% rm_dead_twice, ]
}

x <- bciex::bci12vft_mini %>%
  dplyr::rename(QX = x, QY = y) %>%
  rlang::set_names(tolower) %>%
  check_single_plotid()

rm_dead_twice(x, cns = "plotcensusnumber")







# map_quad() -------------------------------------------------------------

library(dplyr)

# Fixing wrong names
vft <- rename(bciex::bci12vft_mini, QX = x, QY = y)

# Filter the data you want
filtered <- dplyr::filter(
  vft,
  PlotID == 1,
  CensusID == max(CensusID, na.rm = TRUE),
  DBH > 10,
)

# Save time by viewing the output of only one quadrat
one_quadrat <- top(filtered, QuadratID)
map_quad(one_quadrat)

# Storing in `p` all the maps
p <- map_quad(filtered)

# Visualizing the first plot
first(p)

# Printing to .pdf with parameters optimized for size letter
pdf("default-map.pdf", paper = "letter", height = 10.5, width = 8)
p
dev.off()



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



