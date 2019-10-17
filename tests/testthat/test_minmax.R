context("Running minmax")

x <- cumsum(rnorm(100))
minmax_test <- function(price, what = "max", na_rm = TRUE) {
  last_min <- price[1]
  last_max <- price[1]
  temp_min <- price[1]
  temp_max <- price[1]

  maxes <- rep(price[1], length(price))
  mins  <- rep(price[1], length(price))
  for (t in 2:length(price)) {
    prev_price <- price[t - 1]
    actual_price <- price[t]

    if (prev_price > last_max & actual_price < prev_price) {
      last_max <- prev_price
      last_min <- temp_min
      temp_min <- actual_price
    } else if (prev_price < last_min & actual_price > prev_price) {
      last_min <- prev_price
      last_max <- temp_max
      temp_max <- actual_price
    }

    if (actual_price < temp_min)
      temp_min <- actual_price

    if (actual_price > temp_max)
      temp_max <- actual_price

    maxes[t] <- last_max
    mins[t]  <- last_min
  }
  if (what == "max") {
    maxes
  } else if (what == "min") {
    mins
  }
}

test_that("correct minmax", {
  expect_identical(
    minmax_run(x, metric = "min"),
    minmax_test(x, what = "min")
  )

  expect_identical(
    minmax_run(x, metric = "max"),
    minmax_test(x, what = "max")
  )

})

test_that("minmax dealing with NA", {
  x[sample(1:100, 20)] <- NA
  expect_silent(minmax_run(x, metric = "min"))
  expect_silent(minmax_run(x, metric = "max"))
})
