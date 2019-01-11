## Test environments

* local windows 10 x64, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results


Platform:	Windows Server 2008 R2 SP1, R-devel, 32/64 bit
Build ID:	fgeo.plot_0.0.0.9402.tar.gz-2533537420614cdea73725cd4b3af12f

ADDRESS
* Version contains large components (0.0.0.9402)
* Possibly mis-spelled words in DESCRIPTION:
  ForestGEO (2:13, 12:19)

NOTES:
Examples with CPU or elapsed time > 5s
                              user system elapsed
plot_tag_status_by_subquadrat 7.00   0.07    7.12
autoplot_by_species.sp_elev   5.19   0.02    5.20
autoplot.sp_elev              4.75   0.08    5.05

WARNINGS:
* New submission
* Version contains large components (0.0.0.9402)
* Possibly mis-spelled words in DESCRIPTION: ForestGEO (2:13, 12:19)
* Unknown, possibly mis-spelled, fields in DESCRIPTION: 'Remotes'
Strong dependencies not in mainstream repositories: fgeo.tool, fgeo.x

## Downstream dependencies

There are currently no downstream dependencies for this package.
