
-   [Runner](#runner)
    -   [Installation](#installation)
    -   [Examples](#examples)
        -   [window\_run](#window_run)
        -   [min\_run and max\_run](#min_run-and-max_run)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Runner
======

[![Travis-CI Build Status](https://travis-ci.org/elo2zero/runner.svg?branch=master)](https://travis-ci.org/elo2zero/runner) [![MIT License](https://badges.frapsoft.com/os/mit/mit.svg)](https://opensource.org/licenses/mit-license.php) [![Coverage status](https://codecov.io/gh/elo2zero/runner/branch/master/graph/badge.svg)](https://codecov.io/github/elo2zero/runner?branch=master)

Running functions for R vector writen in Rcpp

Installation
------------

You can install runner from github with:

Examples
--------

The main idea of the package is to provide running operations on R vectors. Running functions are these which are applied to all elements up to actual one. Typical example implemented already in `base` are `cumsum`, `cummean`, `cummin` etc. Functions provided in this package works similar but with extended functionality such as handling `NA` and custom window size. Most functions provided in package are basing on the same logic 1. `k` window size denotes number of elements from i-th backwards, where functions are calculated.
(obrazek pokazujący ruchome okno) 2. `na_rm` handling missing equivalent to `na.rm`. In case of running functions, `NA` is replaced with last finite value. 3. `na_pad` if window size exceeds number of available elements, than first `k-1` are filled with `NA`. 4. `which` In case of running index

### window\_run

Function creates list of windows. Because `runner` doesn't provide limited functionality, one can calculate own running-window-statistics oneself.

    #> [[1]]
    #> [1] 1
    #> 
    #> [[2]]
    #> [1] 1 2
    #> 
    #> [[3]]
    #> [1] 1 2 3
    #> 
    #> [[4]]
    #> [1] 2 3 4
    #> 
    #> [[5]]
    #> [1] 4 5

Such windows can be used in further calculations, with any R function.

    #> [1] 1 3 6 9 9

### min\_run and max\_run

``` r
library(ggplot2)
set.seed(11)
x1 <- rnorm( 30 ) %>% cumsum
x2 <- sample( c( rep(NA,10), rnorm(20)) ) %>% cumsum
k  <- sample( 1:4, 30, replace=TRUE) %>% cumsum

min1 <- min_run(x1)
min2 <- min_run(x2, na_rm = TRUE)
min3 <- min_run(x2, na_rm = TRUE, k=4)
min4 <- min_run(x2, na_rm = FALSE, k=k)

max1 <- max_run(x1)
max2 <- max_run(x2, na_rm = TRUE)
max3 <- max_run(x2, na_rm = TRUE, k=4)
max4 <- max_run(x2, na_rm = FALSE, k=k)

data.frame( x1, min1, i = 1:30) %>%
  ggplot(aes(x=i, y=x1)) +
  geom_line() +
  geom_point(aes(y = min1), color="red") +
  geom_point(aes(y = max1), color="blue") +
  ggtitle("running minimum")
```

![](README-min_run_example-1.png)
