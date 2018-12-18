git checkout master
git fetch upstream
git reset --hard upstream/master



message="Prune documentation
* Organize roxygen2 docs."

git add .
git commit --amend -m "$message"

git add .
git commit --amend -m "$message"
