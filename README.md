
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

[Install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo/index.html#installation)

    # install.packages("remotes")
    remotes::install_github("forestgeo/fgeo.map")

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

``` r
library(fgeo.map)

census <- fgeo.data::luquillo_tree6_random
some_sp <- subset(census, sp %in% c("PREMON", "CASARB"))
map_sp_elev(some_sp)
```

<img src="README-fgeo.map-sp-1.png" width="98%" style="display: block; margin: auto;" />

[Get started with
**fgeo**](https://forestgeo.github.io/fgeo/articles/fgeo.html)

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgments

For ideas and guidance, thanks to Suzanne Lao, Stuart Davis, Shameema
Jafferjee Esufali, David Kenfack and Anudeep Singh.
