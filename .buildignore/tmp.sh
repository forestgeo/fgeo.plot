# Release 

git status
git add .
git commit -a -m "Update site"

git tag -a  VERSION-GOES-HERE -m "Pre-release"
git checkout  VERSION-GOES-HERE
git push origin  VERSION-GOES-HERE

git checkout pre-release
git merge VERSION-GOES-HERE
git pull
git push -u origin pre-release



for file in ../fgeo.base/ship.sh ../fgeo.data/ship.sh ../fgeo.demography/ship.sh ../fgeo.habitat/ship.sh ../fgeo.plot/ship.sh ../fgeo.tool/ship.sh;
do
  cp ship.sh $file;
done



grep -R "fgeo.plot" ../fgeo/R ../fgeo.abundance/R ../fgeo.base/R ../fgeo.data/R ../fgeo.demography/R ../fgeo.habitat/R ../fgeo.tool/R

grep -R "fgeo.plot" ../fgeo/README.Rmd ../fgeo.abundance/README.Rmd ../fgeo.base/README.Rmd ../fgeo.data/README.Rmd ../fgeo.demography/README.Rmd ../fgeo.habitat/README.Rmd ../fgeo.tool/README.Rmd

grep -R "fgeo.plot" ../fgeo/vignettes ../fgeo.abundance/vignettes ../fgeo.base/vignettes ../fgeo.data/vignettes ../fgeo.demography/vignettes ../fgeo.habitat/vignettes ../fgeo.tool/vignettes

