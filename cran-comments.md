## Test environments

* local windows 10 x64, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

Status: 2 NOTEs
* using R version 3.4.4 (2018-03-15)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using option ‘--as-cran’
* this is package ‘fgeo.plot’ version ‘1.1.1.9000’
NOTE
* Examples with CPU or elapsed time > 5s
* user system elapsed
* autoplot.sp_elev 4.628 0.116 13.634
* plot_tag_status_by_subquadrat 3.912 0.108 11.136
* plot_dbh_bubbles_by_quadrat 2.888 0.012 8.453
* autoplot_by_species.sp_elev 2.808 0.036 8.061

NOTE (safe to ignore)
* New submission
* Version contains large components (1.1.1.9000)
* Strong dependencies not in mainstream repositories:
* fgeo.tool
* Suggests or Enhances not in mainstream repositories:
* fgeo.x
* Availability using Additional_repositories specification:
* fgeo.tool yes https://forestgeo.github.io/drat/
* fgeo.x yes https://forestgeo.github.io/drat/

## Downstream dependencies

I checked 1 reverse dependencies (0 from CRAN + 1 from <https://forestgeo.github.io/drat/>; summary at <https://github.com/forestgeo/fgeo.plot/tree/master/revdep>):

 * I saw 0 new problems
 * I failed to check 0 packages
