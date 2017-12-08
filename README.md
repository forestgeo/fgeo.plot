
<!-- Don't edit README.md; instead, edit README.Rmd -->
map <img src="https://i.imgur.com/39pvr4n.png" align="right" height=44 />
=========================================================================

[![Travis-CI Build Status](https://travis-ci.org/forestgeo/map.svg?branch=master)](https://travis-ci.org/forestgeo/map) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/map)](https://cran.r-project.org/package=map)

Tools to map ForestGEO's data.
------------------------------

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

After installing and loading **map**, get help about the main function with `?map_tag()`.

Example
-------

``` r
library(dplyr)
library(ggplot2)
library(map)

# Filter the plot you want to map
vft <- dplyr::filter(map::bci12vft_mini, PlotID == 1)

# This data set has two wrong names that need to be fixed before using map_tag()
vft_rnm <- dplyr::rename(vft, qx = x, qy = y)
maps <- map_tag(vft_rnm)
#> Warning in map_tag(vft_rnm): Multiple censuses were detected
#>   * Filtering only the greatest `CensusID`

# Plotting only one map to screen
maps[1]
#> $`4915_1`
```

<img src="README-example-map-1.png" width="98%" style="display: block; margin: auto;" />
