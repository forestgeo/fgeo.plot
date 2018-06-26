
<!-- Don't edit README.md; instead, edit README.Rmd -->

# <img src="https://i.imgur.com/m8FNhQR.png" align="right" height=88 /> Map species, trees and topography.

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build
Status](https://travis-ci.org/forestgeo/fgeo.map.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.map)
[![Coverage
status](https://codecov.io/gh/forestgeo/fgeo.map/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/fgeo.map?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/fgeo.map)](https://cran.r-project.org/package=fgeo.map)

## Installation

    # install.packages("remotes")
    remotes::install_github("EDIT-OWNER/EDIT-PACKAGE-NAME")

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(fgeo)
#> -- Attaching packages ------------------------------------------------ fgeo 0.0.0.9000 --
#> v bciex           0.0.0.9000     v fgeo.demography 0.0.0.9000
#> v fgeo.abundance  0.0.0.9004     v fgeo.habitat    0.0.0.9006
#> v fgeo.base       0.0.0.9001     v fgeo.map        0.0.0.9204
#> v fgeo.data       0.0.0.9002     v fgeo.tool       0.0.0.9003
#> -- Conflicts -------------------------------------------------------- fgeo_conflicts() --
#> x dplyr::filter()    masks stats::filter()
#> x dplyr::intersect() masks base::intersect()
#> x dplyr::lag()       masks stats::lag()
#> x dplyr::setdiff()   masks base::setdiff()
#> x dplyr::setequal()  masks base::setequal()
#> x dplyr::union()     masks base::union()
```

Map species’ distribution.

``` r
census <- luquillo_tree6_random
elevation <- fgeo_elevation(luquillo_elevation)

p <- maply_sp_elev(census, luquillo_elevation, species = c("PREMON", "CASARB"))
# Show first map only
p[[1]]
```

<img src="README-fgeo.map-sp-1.png" width="98%" style="display: block; margin: auto;" />

Map tree tags by status, showing four subquadrats per plot-page.

``` r
# Fix two wrong names
vft <- luquillo_vft_4quad
vft <- pick_top(vft, CensusID, -1)  # pick from the bottom of the rank
vft <- pick_top(vft, PlotID)

# All maps
p2 <- maply_tag(vft)
length(p2)
#> [1] 16

# Show first map
p2[[1]]
```

<img src="README-map-tag-1.png" width="98%" style="display: block; margin: auto;" />

Map trees within a quadrat mapping tree diameter to point size.

``` r
# Focusing on trees which dbh is 10 mm or more
vft2 <- pick_dbh_min(vft, 10)

# All maps
p3 <- maply_quad(vft2)
#> * Appending tags of dead trees with the suffix '.d'
#> * Standarizing `dbh` by the count of `dbh` measurements
length(p3)
#> [1] 4

# Show first map
first(p3)
#> Warning: Removed 57 rows containing missing values (geom_point).
```

<img src="README-map-quad-1.png" width="98%" style="display: block; margin: auto;" />

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgments

For ideas and guidance, thanks to Suzanne Lao, Stuart Davis, Shameema
Jafferjee Esufali, David Kenfack and Anudeep Singh.
