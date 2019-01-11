devtools::install()

update.packages()

pkgs <- paste0("fgeo", c(".tool", ".analyze", ".plot", ".krig", ".misc", ""))
vet::vet(pkgs, devtools::check)

devtools::check_win_devel()

devtools::check_rhub()

rhub::check_for_cran(email = "maurolepore@gmail.com")
