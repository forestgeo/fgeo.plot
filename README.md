
<!-- Don't edit README.md; instead, edit README.Rmd -->
fgeo.map: map ForestGEO's data <img src="https://i.imgur.com/39pvr4n.png" align="right" height=44 />
====================================================================================================

[![Build Status](https://travis-ci.org/forestgeo/fgeo.map.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.map) [![Coverage status](https://codecov.io/gh/forestgeo/fgeo.map/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/fgeo.map?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/fgeo.map)](https://cran.r-project.org/package=fgeo.map)

Installation
------------

Install **fgeo.map** from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github(repo = "forestgeo/fgeo.map")
library(fgeo.map)
```

Or with:

``` r
source("https://install-github.me/forestgeo/fgeo.map")
```

Example
-------

``` r
library(fgeo.map)
library(bciex)
library(fgeo.tool)
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

<img src="README-fgeo.map-sp-1.png" width="98%" style="display: block; margin: auto;" />

Map tree tags by status, showing four subquadrats per plot-page.

``` r
# Fix two wrong names
viewfulltable <- rename(bci12vft_mini, qx = x, qy = y)

# Filter one plot and one census
vft <- filter(viewfulltable, PlotID == 1, CensusID == 6)

# All maps
p2 <- map_tag(vft)
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
#> Warning in fgeo.tool::str_suffix_match(crucial$tag, crucial$status,
#> status_d, : No stem has status `dead`. Is this what you expect?
#> * Standarizing `dbh` by the count of `dbh` measurements
length(p3)
#> [1] 10

# Show first map
first(p3)
```

<img src="README-map-quad-1.png" width="98%" style="display: block; margin: auto;" />
