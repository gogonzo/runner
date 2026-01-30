
# runner <img src="man/figures/hexlogo.png" align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/gogonzo/runner/workflows/R-CMD-check/badge.svg)](https://github.com/gogonzo/runner/actions)
[![CRAN](https://cranlogs.r-pkg.org/badges/runner)](https://CRAN.R-project.org/package=runner)
<!-- badges: end -->

Lightweight R package for rolling window operations. Apply any R
function on rolling windows with full control over window size, lag, and
time indices. Works with equally and unequally spaced time series.

## Installation

``` r
# CRAN
install.packages("runner")

# GitHub
devtools::install_github("gogonzo/runner")
```

## Example

``` r
library(runner)

# Rolling mean with window size 3
runner(1:10, k = 3, f = mean)
```

    ##  [1] 1.0 1.5 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0

``` r
# Rolling regression on time series data
df <- data.frame(
  date = Sys.Date() + cumsum(sample(1:3, 20, TRUE)),
  y = rnorm(20),
  x = rnorm(20)
)

runner(
  x = df,
  k = "7 days",
  idx = df$date,
  f = function(data) coef(lm(y ~ x, data = data))[2]
)
```

    ##           x           x           x           x           x           x           x           x           x           x           x           x           x           x           x           x 
    ##          NA  5.97481674  1.49452100 -0.21575866  0.72053778  0.70082162  1.24506110  1.04075030  1.08250746  0.64303239  0.54936879  0.58570439 -0.78182096 -0.14576205 -0.49775101 -0.28316471 
    ##           x           x           x           x 
    ##  4.25927423  1.08657714  0.04363541 -0.15253300
