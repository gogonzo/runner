# runner 0.2.3
* added minmax_run which calculates running mins and maxes to denote current highs and lows

# runner 0.2.2
* added lag to all runner functions
* added runner function which is able to apply any R function on running window


# runner 0.2.1
* added runner function which allows to apply custom function on running windows - so far returning only numeric

# runner 0.2.0

## Improvements

* all functions have additional `idx` argument which allows to compute running windows within specified date/time/indexes range.
* added lag_run function - lagged value able to be computed on date/time lag.
* added length_run function to compute number of elements within specified date/time/indexes range.
