## Test environments

* local windows 10 x64, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

1.1.0.9000

WARNING
New submission
Version contains large components (1.1.0.9000)
Strong dependencies not in mainstream repositories:
  fgeo.tool
Suggests or Enhances not in mainstream repositories:
  fgeo.x
Availability using Additional_repositories specification:
  fgeo.tool   yes   https://forestgeo.github.io/drat/
  fgeo.x      yes   https://forestgeo.github.io/drat/

NOTE
Examples with CPU or elapsed time > 5s
                               user system elapsed
plot_tag_status_by_subquadrat 3.872  0.060  10.835
plot_dbh_bubbles_by_quadrat   2.856  0.020   7.676
autoplot_by_species.sp_elev   2.732  0.016   8.140
autoplot.sp_elev              2.004  0.052   6.973

## Downstream dependencies

I checked 1 reverse dependencies (0 from CRAN + 1 from <https://forestgeo.github.io/drat/>; summary at <https://github.com/forestgeo/fgeo.plot/tree/master/revdep>):

 * I saw 0 new problems
 * I failed to check 0 packages
