
# Dev ---------------------------------------------------------------------

library(devtools)

# create("pkg")
use_git()
use_github()

library(devtools)

use_gpl3_license()

use_github_links()

use_roxygen_md()

use_package_doc()

use_readme_rmd()
# then
# * Shortly describe pkg name
# * Next to short description paste code of snippet logo
# * paste badges under
use_travis()
# then at https://travis-ci.org/profile/forestgeo sync, search and activate pkg
use_coverage("coveralls")
# then at https://coveralls.io/repos/new sync, search and activate pkg
use_cran_badge()

use_news_md()

* Use/update snippet pkgdown. Save as `./_pkgdown.yml`

DESCRIPTION

Authors@R: use snippet author

Depends:
  R (>= 3.1.2)

### Adding a new package to fgeo

* Add to fgeo.R in right section
* Add to `Imports:` or Suggests:` with `use_package()`
* Add to remotes
* Add to _pkgdown.yml under:
  * references (package index)
* articles (tutorials menu)











# map_quad() -------------------------------------------------------------

library(dplyr)
library(fgeo.utils)

p <- map_quad(one_quadrat)
dplyr::first(p)

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




# map_Tag() ---------------------------------------------------------------

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


# This data set has two wrong names that need to be fixed before using map_Tag()
vft1_rnm <- dplyr::rename(vft1, qx = x, qy = y)
maps <- map_Tag(vft1_rnm)

# Plotting only one map to screen
maps[1]



