
<!-- Don't edit README.md; instead, edit README.Rmd -->
map: map ForestGEO's data <img src="https://i.imgur.com/39pvr4n.png" align="right" height=44 />
===============================================================================================

[![Build Status](https://travis-ci.org/forestgeo/map.svg?branch=master)](https://travis-ci.org/forestgeo/map) [![Coverage status](https://codecov.io/gh/forestgeo/map/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/map?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/map)](https://cran.r-project.org/package=map)

Installation
------------

Install **map** from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github(repo = "forestgeo/map")
library(map)
```

Or with:

``` r
source("https://install-github.me/forestgeo/map")
```

Example
-------

``` r
library(map)
library(bciex)
library(fgeo.utils)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

Map species' distribution.

``` r
census <- bci12t7mini

# All maps
p <- map_sp(census, c("faraoc", "hybapr"))

# Show first map
first(p)
```

<img src="README-map-sp-1.png" width="98%" style="display: block; margin: auto;" />

Map tree tags by status, showing four subquadrats per plot-page.

``` r
# Fix two wrong names
viewfulltable <- rename(bci12vft_mini, qx = x, qy = y)

# Filter one plot and one census
vft <- filter(viewfulltable, PlotID == 1, CensusID == 6)

# All maps
p2 <- map_tag(vft)
#> Warning in check_valid_status(x, .status = c(status_d, status_a), "status"): No observation has .status = D, A
#>   * Valid values: alive, dead
length(p2)
#> [1] 40

# Show first map
first(p2)
```

<img src="README-map-tag-1.png" width="98%" style="display: block; margin: auto;" />

Map trees within a quadrat mapping tree diameter to point size.

``` r
# Filtering:
# * Trees of diameter greater than 10 cm;
# * Last census;
# * Plot 1.
vft2 <- filter(
  viewfulltable,
  DBH > 10,
  CensusID == max(CensusID, na.rm = TRUE),
  PlotID == 1
)

# All maps
p3 <- map_quad(vft2)
#> * Appending tags of dead trees with the suffix '.d'
#> Warning in tag_dead(crucial$tag, crucial$status): No stem is dead. Is that
#> what you expect?
#> * Standarizing `dbh` by the count of `dbh` measurements
length(p3)
#> [1] 10

# Show first map
first(p3)
```

<img src="README-map-quad-1.png" width="98%" style="display: block; margin: auto;" />
