git checkout master
git fetch upstream
git reset --hard upstream/master



<<<<<<< HEAD
message="Prune documentation
* Organize roxygen2 docs."
=======
message="Prune documentation. Also:
* Refactor."
>>>>>>> prune-doc

git add .
git commit --amend -m "$message"

git add .
git commit --amend -m "$message"
