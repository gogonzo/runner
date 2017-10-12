
<!-- README.md is generated from README.Rmd. Please edit that file -->
runner
======

[![Travis-CI Build Status](https://travis-ci.org/elo2zero/runner.svg?branch=master)](https://travis-ci.org/elo2zero/runner) [![Coverage status](https://codecov.io/gh/elo2zero/runner/branch/master/graph/badge.svg)](https://codecov.io/github/elo2zero/runner?branch=master)

Running functions for R vector writen in Rcpp

Installation
------------

You can install runner from github with:

``` r
# install.packages("devtools")
devtools::install_github("elo2zero/runner")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
x1 <- sample( c(1,2,3), 15, replace=TRUE)
x2 <- sample( c(NA,1,2,3), 15, replace=TRUE)
k  <- sample( 1:4, 15, replace=TRUE)
min_run(x1) # simple cumulative minimum
#>  [1] 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1
min_run(x2, na_rm = TRUE) # cumulative minimum with removing NA.
#>  [1] NA  1  1  1  1  1  1  1  1  1  1  1  1  1  1
min_run(x2, na_rm = TRUE, k=4) # minimum in 4-element window
#>  [1] NA  1  1  1  1  1  1  1  1  1  1  1  1  1  1
min_run(x2, na_rm = FALSE, k=k) # minimum in varying k window size
#>  [1] NA NA NA NA  2  1  1 NA  3  2 NA  1  1 NA NA
```
