
<!-- README.md is generated from README.Rmd. Please edit that file -->
runner
======

[![Travis-CI Build Status](https://travis-ci.org/elo2zero/runner.svg?branch=master)](https://travis-ci.org/elo2zero/runner) [![MIT License](https://badges.frapsoft.com/os/mit/mit.svg)](https://opensource.org/licenses/mit-license.php) [![Coverage status](https://codecov.io/gh/elo2zero/runner/branch/master/graph/badge.svg)](https://codecov.io/github/elo2zero/runner?branch=master)

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
library(runner)
set.seed(11)
x1 <- sample( c(1,2,3), 15, replace=TRUE)
x2 <- sample( c(NA,1,2,3), 15, replace=TRUE)
k  <- sample( 1:4, 15, replace=TRUE)
data.frame(

  simple_min_run = min_run(x1), # simple cumulative minimum
  narm_min_run = min_run(x2, na_rm = TRUE), # cumulative minimum with removing NA.
  kconst_min_run = min_run(x2, na_rm = TRUE, k=4), # minimum in 4-element window
  kvary_min_run = min_run(x2, na_rm = FALSE, k=k) # minimum in varying k window size
)
#>    simple_min_run narm_min_run kconst_min_run kvary_min_run
#> 1               1            2              2             2
#> 2               1            1              1             1
#> 3               1            1              1             1
#> 4               1            1              1            NA
#> 5               1            1              1            NA
#> 6               1            1              1            NA
#> 7               1            1              1            NA
#> 8               1            1              1             1
#> 9               1            1              1             1
#> 10              1            1              1            NA
#> 11              1            1              1             1
#> 12              1            1              1             1
#> 13              1            1              1            NA
#> 14              1            1              1            NA
#> 15              1            1              1            NA
```
