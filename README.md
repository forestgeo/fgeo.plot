
<!-- Don't edit README.md; instead, edit README.Rmd -->
try
===

[![Travis-CI Build Status](https://travis-ci.org/maurolepore/try.svg?branch=master)](https://travis-ci.org/maurolepore/try) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/try)](https://cran.r-project.org/package=try)

Overview
========

Try makes if easy to share code to try new things. It aims to draft a solution to address a very specific issue.

Installation
============

Install a specific ISSUE from GitHub with:

    # install.packages("devtools")
    devtools::install_github(repo = "forestgeo/try@ISSUE")

The argument `repo` of `devtools::install_github()` can be in the format `username/repo#pull` or `username/repo@ref`, where `ref` could be a commit, tag, or branch name, or a call to `github_pull()`.
