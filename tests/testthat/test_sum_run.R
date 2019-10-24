context("Running sum")
set.seed(11)
x1 <- sample(c(1,2,3), 15, replace=T)
x2 <- sample(c(NA,1,2,3), 15, replace=T)
k  <- sample(1:15,15, replace=T)
idx <- cumsum(sample(c(1,2,3,4), 15, replace = TRUE))
lag <- sample(-3:3, 15, replace = TRUE)

test_that("sum_run basic",{
  expect_identical(
    sum_run(x1),
    runner(x1, f = sum)
  )
})

test_that("sum_run with na_rm = TRUE", {
  expect_identical(
    sum_run(x2, na_rm = TRUE),
    runner(x2, f = function(x) sum(x, na.rm = TRUE))
  )
})

test_that("sum_run with na_rm = FALSE na_fill = TRUE", {
  expect_identical(
    sum_run(x2, na_rm = FALSE),
    runner(x2, f = function(x) sum(x, na.rm = FALSE))
  )
})

test_that("sum_run lagged", {
  expect_identical(
    sum_run(x2, lag = 3, na_rm = TRUE),
    runner(x2, lag = 3, f = function(x) sum(x, na.rm = TRUE))
  )

  expect_identical(
    sum_run(x2, lag = -3, na_rm = TRUE),
    runner(x2, lag = -3, f = function(x) sum(x, na.rm = TRUE))
  )

  expect_identical(
    sum_run(x2, k = 4, lag = 3, na_rm = TRUE),
    runner(x2, k = 4, lag = 3, f = function(x) sum(x, na.rm = TRUE))
  )

  expect_identical(
    sum_run(x2, k = 4, lag = -3, na_rm = TRUE),
    runner(x2, k = 4, lag = -3, f = function(x) sum(x, na.rm = TRUE))
  )

  expect_identical(
    sum_run(x2, k = 4, lag = 15, na_rm = TRUE),
    runner(x2, k = 4, lag = 15, f = function(x) sum(x, na.rm = TRUE))
  )

  expect_identical(
    sum_run(x2, k = 4, lag = -30, na_rm = TRUE),
    runner(x2, k = 4, lag = -30, f = function(x) sum(x, na.rm = TRUE))
  )

  expect_identical(
    sum_run(x2, k = 4, lag = 30, na_rm = TRUE),
    runner(x2, k = 4, lag = 30, f = function(x) sum(x, na.rm = TRUE))
  )





  expect_equal(
    sum_run(x2, lag = 3, na_rm = TRUE, na_pad = TRUE),
    runner(x2, lag = 3, f = function(x) sum(x, na.rm = TRUE), na_pad = TRUE)
  )

  expect_equal(
    sum_run(x2, lag = -3, na_rm = TRUE, na_pad = TRUE),
    runner(x2, lag = -3, f = function(x) sum(x, na.rm = TRUE), na_pad = TRUE)
  )
})

test_that("sum_run with na_rm = FALSE k = 4", {
  expect_identical(
    sum_run(x2, k = 4, na_rm = FALSE),
    runner(x2, k = 4, f = function(x) sum(x, na.rm = FALSE))
  )
})

test_that("sum_run with na_rm = TRUE k = 4", {
  expect_identical(
    sum_run(x2, k = 4, na_rm = TRUE),
    runner(x2, k = 4, f = function(x) sum(x, na.rm = TRUE))
  )
})

test_that("sum_run with idx++ same as sum_run with windows",{
  expect_identical(sum_run(x1, k = 3),
                   sum_run(x1, k = 3, idx = 0:14))

  expect_identical(sum_run(x1, k = k),
                   sum_run(x1, k = k, idx = 1:15))
})

test_that("sum_run with idx", {
  expect_identical(
    sum_run(x2, k = 4, idx = idx),
    sapply(window_run(x2, k = 4, idx = idx), function(x) {
     if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )

  expect_identical(
    sum_run(x2, k = k, idx = idx),
    sapply(window_run(x2, k = k, idx = idx), function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )

  expect_equal(
    sum_run(x2, k = 4, lag = 3, idx = idx),
    sapply(window_run(x2, k = 4, lag = 3, idx = idx), function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )

  expect_equal(
    sum_run(x2, k = k, lag = 3, idx = idx),
    sapply(window_run(x2, k = k, lag = 3, idx = idx), function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )

  expect_equal(
    sum_run(x2, k = 5, lag = lag, idx = idx),
    sapply(window_run(x2, k = 5, lag = lag, idx = idx), function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )

  expect_equal(
    sum_run(x2, k = k, lag = lag, idx = idx),
    sapply(window_run(x2, k = k, lag = lag, idx = idx), function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )
})

test_that("sum_run with idx negative lag", {
  expect_equal(
    sum_run(x2, k = 4, lag = -3, idx = idx),
    sapply(window_run(x2, k = 4, lag = -3, idx = idx), function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )

  expect_equal(
    sum_run(x2, k = k, lag = -3, idx = idx),
    sapply(window_run(x2, k = k, lag = -3, idx = idx), function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )

  expect_equal(
    sum_run(x2, k = 5, lag = -lag, idx = idx),
    sapply(window_run(x2, k = 5, lag = -lag, idx = idx), function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )

  #### FAILS
  expect_equal(
    sum_run(x2, k = k, lag = -lag, idx = idx),
    sapply(window_run(x2, k = k, lag = -lag, idx = idx), function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    })
  )
})


