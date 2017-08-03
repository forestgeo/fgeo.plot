
<!-- Don't edit README.md; instead, edit README.Rmd -->
try
===

[![DOI](https://zenodo.org/badge/78309322.svg)](https://zenodo.org/badge/latestdoi/78309322)

[![Travis-CI Build Status](https://travis-ci.org/maurolepore/try.svg?branch=master)](https://travis-ci.org/maurolepore/try)

Overview
========

Try makes if easy to share code to try new things. It aims to draft a solution to address a very specific issue, maybe the issue of one user only. That user can access the code by installing the issue-specific snapshot of **try** (GitHub Flow: <https://goo.gl/vzrfgy>), which may be associated to one pull request. This should avoids sharing code, from developer to user, by email.

Installation
============

<<<<<<< HEAD
``` r
# install.packages("devtools")
devtools::install_github("maurolepore/try")  # install branch master
# devtools::install_github("maurolepore/try", ref = "dev")  # install branch dev
```

Explain branch model.
||||||| merged common ancestors
``` r
# install.packages("devtools")
devtools::install_github("maurolepore/try")  # install branch master
# devtools::install_github("maurolepore/try", ref = "dev")  # install branch dev
```
=======
Example installing branch with reference @some-issue:

    # install.packages("devtools")
    devtools::install_github(repo = "forestgeo/try@some-issue")

The argument `repo` of `devtools::install_github()` can be in the format username/repo\[@ref|\#pull\], where `ref` could be a commit, tag, or branch name, or a call to `github_pull()`.
>>>>>>> #142
